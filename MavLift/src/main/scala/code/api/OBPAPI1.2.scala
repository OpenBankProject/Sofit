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

import net.liftweb.http.JsonResponse
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
import code.api.OBPRestHelper

case class APIInfoJSON(
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
  id : String,
  short_name : String,
  full_name : String,
  logo : String,
  website : String
)
case class ViewJSON(
  id : String,
  short_name : String,
  description : String,
  is_public : Boolean
)
case class AccountsJSON(
  accounts : List[AccountJSON]
)
case class AccountJSON(
  id : String,
  label : String,
  views_available : Set[ViewJSON],
  bank_id : String
)
case class ModeratedAccountJSON(
  id : String,
  label : String,
  number : String,
  owners : List[UserJson],
  `type` : String,
  balance : AmountOfMoneyJSON,
  IBAN : String,
  views_available : Set[ViewJSON],
  bank_id : String
)
case class UserJson(
  user_id : String,
  user_provider : String,
  display_name : String
)
case class AmountOfMoneyJSON(
  currency : String,
  amount : String
)
case class TransactionImagesJSON(
  images : List[TransactionImageJSON])
case class TransactionImageJSON(
  id : String,
  label : String,
  URL : String,
  date : Date,
  user : UserJson)

object JSONFactory{
  def stringOrNull(text : String) =
    if(text.isEmpty)
      null
    else
      text

  def stringOptionOrNull(text : Option[String]) =
    text match {
      case Some(t) => stringOrNull(t)
      case _ => null
    }

  def createBankJSON(bank : Bank) : BankJSON = {
    new BankJSON(
      stringOrNull(bank.permalink),
      stringOrNull(bank.shortName),
      stringOrNull(bank.fullName),
      stringOrNull(bank.logoURL),
      stringOrNull(bank.website)
    )
  }

  def createViewJSON(view : View) : ViewJSON = {
    new ViewJSON(
      view.permalink,
      stringOrNull(view.name),
      stringOrNull(view.description),
      view.isPublic
    )
  }

  def createAccountJSON(account : BankAccount, viewsAvailable : Set[ViewJSON] ) : AccountJSON = {
    new AccountJSON(
      account.permalink,
      stringOrNull(account.label),
      viewsAvailable,
      account.bankPermalink
    )
  }

  def createModeratedAccountJSON(account : ModeratedBankAccount, viewsAvailable : Set[ViewJSON]) : ModeratedAccountJSON =  {
    val bankName = account.bankName.getOrElse("")
    new ModeratedAccountJSON(
      account.id,
      stringOptionOrNull(account.label),
      stringOptionOrNull(account.number),
      createOwnersJSON(account.owners.getOrElse(Set()), bankName),
      stringOptionOrNull(account.accountType),
      createAmountOfMoneyJSON(account.currency.getOrElse(""), account.balance),
      stringOptionOrNull(account.iban),
      viewsAvailable,
      stringOptionOrNull(account.bankPermalink)
    )
  }
  
  def createUserJSON(user : User) : UserJson = {
    new UserJson(
          user.id_,
          stringOrNull(user.provider),
          stringOrNull(user.emailAddress)
        )
  }
  
  def createUserJSON(user : Box[User]) : UserJson = {
    user match {
      case Full(u) => createUserJSON(u)
      case _ => null
    }
  }
  
  def createOwnersJSON(owners : Set[AccountOwner], bankName : String) : List[UserJson] = {
    owners.map(o => {
        new UserJson(
          o.id,
          stringOrNull(bankName),
          stringOrNull(o.name)
        )
      }
    ).toList
  }
  def createAmountOfMoneyJSON(currency : String, amount  : String) : AmountOfMoneyJSON = {
    new AmountOfMoneyJSON(
      stringOrNull(currency),
      stringOrNull(amount)
    )
  }
  
  def createTransactionImagesJson(images : List[TransactionImage]) : TransactionImagesJSON = {
    new TransactionImagesJSON(images.map(createTransactionImageJSON))
  }
  
  def createTransactionImageJSON(image : TransactionImage) : TransactionImageJSON = {
    new TransactionImageJSON(
        id = image.id_,
        label = image.description,
        URL = image.imageUrl.toString,
        date = image.datePosted,
        user = createUserJSON(image.postedBy))
  }
}

object OBPAPI1_2 extends OBPRestHelper with Loggable {

  implicit def errorToJson(error: ErrorMessage): JValue = Extraction.decompose(error)
  implicit def successToJson(success: SuccessMessage): JValue = Extraction.decompose(success)

  val dateFormat = ModeratedTransaction.dateFormat
  val apiPrefix = "obp" / "v1.2"

  private def bankAccountsListToJson(bankAccounts: List[BankAccount], user : Box[User]): JValue = {
    val accJson : List[AccountJSON] = bankAccounts.map( account => {
        val views = account permittedViews user
        val viewsAvailable : Set[ViewJSON] =
            views.map( v => {
              JSONFactory.createViewJSON(v)
            })
        JSONFactory.createAccountJSON(account,viewsAvailable)
      })

    val accounts = new AccountsJSON(accJson)
    Extraction.decompose(accounts)
  }


  serve(apiPrefix prefix {
    case Nil JsonGet json => {

      val apiDetails : JValue = {
        val hostedBy = new HostedBy("TESOBE", "contact@tesobe.com", "+49 (0)30 8145 3994")
        val apiInfoJSON = new APIInfoJSON("1.2", gitCommit, hostedBy)
        Extraction.decompose(apiInfoJSON)
      }

      JsonResponse(apiDetails)
    }
  })
  serve(apiPrefix prefix{
    case "banks" :: Nil JsonGet json => {

      def banksToJson(banksList : List[Bank]) : JValue = {
        val banksJSON : List[BankJSON] = banksList.map( b => {
            JSONFactory.createBankJSON(b)
          })
        val banks = new BanksJSON(banksJSON)
        Extraction.decompose(banks)
      }

      JsonResponse(banksToJson(Bank.all))
    }
  })
  serve(apiPrefix prefix {
    case "banks" :: bankId :: "accounts" :: Nil JsonGet json => {

      Bank(bankId) match {
        case Full(bank) => {
          if (isThereAnOAuthHeader)
          {
            val (httpCode, message, oAuthParameters) = validator("protectedResource", httpMethod)
            if(httpCode == 200)
            {
              val user = getUser(httpCode,oAuthParameters.get("oauth_token"))
              val availableAccounts = bank.accounts.filter(_.permittedViews(user).size!=0)
              JsonResponse(bankAccountsListToJson(availableAccounts, user))
            }
            else
              errorJsonResponse(message,httpCode)
          }
          else
          {
              val availableAccounts = bank.publicAccounts
              JsonResponse(bankAccountsListToJson(availableAccounts, None))
          }
        }
        case _ =>  {
          val error = "bank " + bankId + " not found"
          JsonResponse(ErrorMessage(error), Nil, Nil, 400)
        }
      }
    }
  })
  serve(apiPrefix prefix {
    case "banks" :: bankId :: "accounts" :: "private" :: Nil JsonGet json => {

      if (isThereAnOAuthHeader)
      {
        val (httpCode, message, oAuthParameters) = validator("protectedResource", httpMethod)
        if(httpCode == 200)
          getUser(httpCode,oAuthParameters.get("oauth_token")) match {
            case Full(user) => {
              Bank(bankId) match {
                case Full(bank) => {
                    val availableAccounts = bank.nonPublicAccounts(user)
                    JsonResponse(bankAccountsListToJson(availableAccounts, Full(user)))
                }
                case _ =>  {
                  val error = "bank " + bankId + " not found"
                  errorJsonResponse("error", 400)
                }
              }
            }
            case _ => errorJsonResponse("user not found", 400)
          }
        else
          errorJsonResponse(message,httpCode)
      }
      else
        oauthHeaderRequiredJsonResponce
    }
  })
  serve(apiPrefix prefix {
    case "banks" :: bankId :: "accounts" :: "public" :: Nil JsonGet json => {

      Bank(bankId) match {
        case Full(bank) => {
            val availableAccounts = bank.publicAccounts
            JsonResponse(bankAccountsListToJson(availableAccounts, None))
        }
        case _ =>  {
          val error = "bank " + bankId + " not found"
          errorJsonResponse("error", 400)
        }
      }
    }
  })

  serve(apiPrefix prefix {
    case "banks" :: bankId :: "accounts" :: accountId :: viewId :: "account" :: Nil JsonGet json => {

      def getBankAccountDetail(bankId : String, accountId : String, viewId : String, user : Box[User]) : Box[(ModeratedBankAccount, Set[View])] = {
        for {
          bank <- Bank(bankId)
          account <- BankAccount(bankId, accountId)
          availableviews <- Full(account.permittedViews(user))
          view <- View.fromUrl(viewId)
          moderatedAccount <- account.moderatedBankAccount(view, user)
        } yield (moderatedAccount, availableviews)
      }

      def accountDetailJsonResponse(accountDetails : Box[(ModeratedBankAccount, Set[View])]) : JsonResponse = {

        accountDetails match {
          case Full(details) => {
            val views = details._2
            val account = details._1

            val viewsAvailable : Set[ViewJSON] =
                views.map( v => {
                  JSONFactory.createViewJSON(v)
                })
            val moderatedAccount = JSONFactory.createModeratedAccountJSON(account,viewsAvailable)

            val accountInJson = Extraction.decompose(moderatedAccount)
            JsonResponse(accountInJson, Nil, Nil, 200)
          }
          case Failure(msg, _, _) => errorJsonResponse(msg,400)
          case _ => errorJsonResponse("error",400)
        }
      }

      if(isThereAnOAuthHeader)
      {
        val (httpCode, message, oAuthParameters) = validator("protectedResource", httpMethod)
        if(httpCode == 200)
        {
          val user = getUser(httpCode,oAuthParameters.get("oauth_token"))
          val accountDetails : Box[(ModeratedBankAccount, Set[View])] = getBankAccountDetail(bankId, accountId, viewId, user)
          accountDetailJsonResponse(accountDetails)
        }
        else
          errorJsonResponse(message,httpCode)
      }
      else
      {
        val accountDetail = getBankAccountDetail(bankId, accountId, viewId, Empty)
        accountDetailJsonResponse(accountDetail)
      }
    }
  })

  serve(apiPrefix prefix {
    case "banks" :: bankId :: "accounts" :: accountId :: viewId :: "transactions" :: transactionId :: "metadata" :: "images" :: Nil JsonGet json => {
      
      failIfBadOauth {
        user: Box[User] =>
          for {
            metadata <- moderatedTransactionMetadata(bankId, accountId, viewId, transactionId, getUser)
            images <- Box(metadata.images) ?~ { "view " + viewId + " does not authorize tags access" }
          } yield {
            val json = JSONFactory.createTransactionImagesJson(images)
            successJsonResponse(Extraction.decompose(json), 200)
          }
      }
      
    }
  })
  
    private def moderatedTransactionMetadata(bankId : String, accountId : String, viewId : String, transactionID : String, user : Box[User]) : Box[ModeratedTransactionMetadata] =
    for {
      account <- BankAccount(bankId, accountId)
      view <- View.fromUrl(viewId)
      moderatedTransaction <- account.moderatedTransaction(transactionID, view, user) ?~ "view/transaction not authorized"
      metadata <- Box(moderatedTransaction.metadata) ?~ {"view " + viewId + " does not authorize metadata access"}
    } yield metadata
}