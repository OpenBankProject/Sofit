/**
Open Bank Project - Transparency / Social Finance Web Application
Copyright (C) 2011, 2012, TESOBE / Music Pictures Ltd

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

Email: contact@tesobe.com
TESOBE / Music Pictures Ltd
Osloerstrasse 16/17
Berlin 13359, Germany

  This product includes software developed at
  TESOBE (http://www.tesobe.com/)
  by
  Simon Redfern : simon AT tesobe DOT com
  Stefan Bethge : stefan AT tesobe DOT com
  Everett Sochowski : everett AT tesobe DOT com
  Ayoub Benali: ayoub AT tesobe DOT com

 */
package code.api.v1_2

import net.liftweb.http._
import net.liftweb.http.rest._
import net.liftweb.json.JsonDSL._
import net.liftweb.json.Printer._
import net.liftweb.json.Extraction
import net.liftweb.json.JsonAST._
import net.liftweb.common.{Failure,Full,Empty, Box, Loggable}
import net.liftweb.mongodb._
import com.mongodb.casbah.Imports._
import _root_.java.math.MathContext
import org.bson.types._
import _root_.net.liftweb.util._
import _root_.net.liftweb.mapper._
import _root_.net.liftweb.util.Helpers._
import _root_.scala.xml._
import _root_.net.liftweb.http.S._
import net.liftweb.mongodb.{ Skip, Limit }
import _root_.net.liftweb.mapper.view._
import com.mongodb._
import code.model.traits._
import code.model.implementedTraits.View
import code.model.implementedTraits.Public
import java.util.Date
import code.api.OAuthHandshake._
import code.model.dataAccess.OBPEnvelope.{OBPOrder, OBPLimit, OBPOffset, OBPOrdering, OBPFromDate, OBPToDate, OBPQueryParam}
import java.net.URL
import code.util.APIUtil._

case class APIInfoJSON(
  api : APIInfo
)
case class APIInfo(
  version : String,
  git_commit : String,
  hosted_by : HostedBy
)
case class HostedBy(
  organisation : String,
  email : String,
  phone : String
)
case class ErrorMessage(
  error : String
)
case class SuccessMessage(
  success : String
)
case class BanksJSON(
  banks : List[BankJSON]
)
case class BankJSON(
  bank : BankInfo
)
case class BankInfo(
  id : String,
  short_name : String,
  full_name : String,
  logo : String,
  website : String
)
case class ViewJSON(
  view : ViewInfo
)
case class ViewInfo(
  id : String,
  short_name : String,
  description : String,
  is_public : Boolean
)
case class AccountsJSON(
  accounts : Set[AccountJSON]
)
case class AccountJSON(
  account : AccountInfo
)
case class AccountInfo(
  id : String,
  label : String,
  views_available : Set[ViewJSON]
)

object JSONFactory{
  def stringOrNull(text : String) =
    if(text.isEmpty)
      null
    else
      text

  def createBankInfo(bank : Bank) : BankInfo = {
    new BankInfo(
      stringOrNull(bank.permalink),
      stringOrNull(bank.shortName),
      stringOrNull(bank.fullName),
      stringOrNull(bank.logoURL),
      stringOrNull(bank.website)
    )
  }

  def createViewInfo(view : View) : ViewInfo = {
    new ViewInfo(
      view.permalink,
      stringOrNull(view.name),
      stringOrNull(view.description),
      view.isPublic
    )
  }

  def createAccountInfo(account : BankAccount, viewsAvailable : Set[ViewJSON] ) : AccountInfo = {
    new AccountInfo(
      account.permalink,
      stringOrNull(account.label),
      viewsAvailable
    )
  }
}

object OBPAPI1_2 extends RestHelper with Loggable {

  implicit def errorToJson(error: ErrorMessage): JValue = Extraction.decompose(error)
  implicit def successToJson(success: SuccessMessage): JValue = Extraction.decompose(success)

  val dateFormat = ModeratedTransaction.dateFormat

  private def bankAccountSetToJson(bankAccounts: Set[BankAccount]): JValue = {
    val accJson : Set[AccountJSON] = bankAccounts.map( account => {
        val views = account permittedViews None
        val viewsAvailable : Set[ViewJSON] =
            views.map( v => {
              new ViewJSON(JSONFactory.createViewInfo(v))
            })
        new AccountJSON(JSONFactory.createAccountInfo(account,viewsAvailable))
      })

    val accounts = new AccountsJSON(accJson)
    Extraction.decompose(accounts)
  }


  serve("obp" / "v1.2" prefix {
    case Nil JsonGet json => {
      logAPICall

      val apiDetails : JValue = {
        val hostedBy = new HostedBy("TESOBE", "contact@tesobe.com", "+49 (0)30 8145 3994")
        val apiInfo = new APIInfo("1.2", gitCommit, hostedBy)
        val APIInfoJSON = new APIInfoJSON(apiInfo)
        Extraction.decompose(APIInfoJSON)
      }

      JsonResponse(apiDetails)
    }
  })
  serve("obp" / "v1.2" prefix{
    case "banks" :: Nil JsonGet json => {
      logAPICall

      def banksToJson(banksList : List[Bank]) : JValue = {
        val banksJSON : List[BankJSON] = banksList.map( b => {
            new BankJSON(JSONFactory.createBankInfo(b))
          })
        val banks = new BanksJSON(banksJSON)
        Extraction.decompose(banks)
      }
      JsonResponse(banksToJson(Bank.all))
    }
  })
  serve("obp" / "v1.2" prefix {
    case "banks" :: bankId :: "accounts" :: "public" :: Nil JsonGet json => {
      logAPICall

      Bank(bankId) match {
        case Full(bank) => {
            val availableAccounts = bank.publicAccounts
            JsonResponse(bankAccountSetToJson(availableAccounts))
        }
        case _ =>  {
          val error = "bank " + bankId + " not found"
          JsonResponse(ErrorMessage(error), Nil, Nil, 400)
        }
      }
    }
  })
  serve("obp" / "v1.2" prefix {
    case "banks" :: bankId :: "accounts" :: "private" :: Nil JsonGet json => {
      logAPICall

      if (isThereAnOAuthHeader)
      {
        val (httpCode, message, oAuthParameters) = validator("protectedResource", httpMethod)
        if(httpCode == 200)
        {
          val user = getUser(httpCode,oAuthParameters.get("oauth_token"))
          Bank(bankId) match {
            case Full(bank) => {
                val availableAccounts = bank.privateAccounts(user)
                JsonResponse(bankAccountSetToJson(availableAccounts))
            }
            case _ =>  {
              val error = "bank " + bankId + " not found"
              JsonResponse(ErrorMessage(error), Nil, Nil, 400)
            }
          }
        }
        else
          errorJsonResponce(message,httpCode)
      }
      else
        oauthHeaderRequiredJsonResponce
    }
  })

}