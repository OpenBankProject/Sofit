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

package code.snippet

import net.liftweb.util.Helpers._
import code.model.traits.BankAccount
import net.liftweb.common.Full
import code.model.dataAccess.OBPUser
import code.model.traits.User
import code.model.implementedTraits.Public
import code.model.implementedTraits.Owner
import code.model.implementedTraits.Board
import code.model.implementedTraits.Authorities
import code.model.implementedTraits.Team
import code.model.implementedTraits.OurNetwork
import net.liftweb.http.SHtml
import scala.xml.Text
import net.liftweb.http.js.JsCmds.Noop
import code.lib.ObpGet
import code.lib.ObpJson._
import net.liftweb.json.JsonAST.{JInt, JArray}
import net.liftweb.json.JsonAST.JField
import net.liftweb.json.JsonAST.JObject
import net.liftweb.json.JsonAST.JString
import net.liftweb.json.JsonAST.JValue
import net.liftweb.json.JBool
import net.liftweb.common.Loggable

class AccountsOverview extends Loggable {
		  		  
  val banksJsonBox = ObpGet.allBanks
  
  val bankJsons : List[BankJson] = banksJsonBox.map(_.bankJsons).toList.flatten
  
  val bankIds = for {
    bank <- bankJsons
    id <- bank.id
  } yield id
  
  logger.info("Accounts Overview: Bank ids found: " + bankIds)
  
  type BankID = String
  val publicAccountJsons : List[(BankID, BarebonesAccountJson)] = for {
    bankId <- bankIds
    publicAccountsJson <- ObpGet("/banks/" + bankId + "/accounts/public").flatMap(_.extractOpt[BarebonesAccountsJson]).toList
    barebonesAccountJson <- publicAccountsJson.accounts.flatten
  } yield (bankId, barebonesAccountJson)
  
  logger.info("Accounts Overview: Public accounts found: " + publicAccountJsons)
  
  val privateAccountJsons : List[(BankID, BarebonesAccountJson)] = for {
    bankId <- bankIds
    privateAccountsJson <- ObpGet("/banks/" + bankId + "/accounts/private").flatMap(_.extractOpt[BarebonesAccountsJson]).toList
    barebonesAccountJson <- privateAccountsJson.accounts.flatten
  } yield (bankId, barebonesAccountJson)
  
  logger.info("Accounts Overview: Private accounts found: " + privateAccountJsons)
  
  def publicAccounts = {

    if (publicAccountJsons.size == 0) {
      ".accountList" #> "No public accounts"
    } else {
      ".accountList" #> publicAccountJsons.map {
        case (bankId, accountJson) => {
          //TODO: It might be nice to ensure that the same view is picked each time the page loads
          val views = accountJson.views_available.flatten
          val aPublicViewId: String = (for {
            aPublicView <- views.filter(view => view.is_public.getOrElse(false)).headOption
            viewId <- aPublicView.id
          } yield viewId).getOrElse("")

          ".accLink *" #> accountJson.label &
            ".accLink [href]" #> {
              val accountId = accountJson.id.getOrElse("")
              "/banks/" + bankId + "/accounts/" + accountId + "/" + aPublicViewId
            }
        }
      }
    }
  }
  
  def authorisedAccounts = {
    def loggedInSnippet(user: User) = {
      
      ".accountList" #> privateAccountJsons.map{case (bankId, accountJson) => {
        //TODO: It might be nice to ensure that the same view is picked each time the page loads
        val views = accountJson.views_available.flatten
        val aPrivateViewId: String = (for {
          aPrivateView <- views.filterNot(view => view.is_public.getOrElse(false)).headOption
          viewId <- aPrivateView.id
        } yield viewId).getOrElse("")
        
        ".accLink *" #> accountJson.label &
        ".accLink [href]" #> {
          val accountId : String = accountJson.id.getOrElse("")
          "/banks/" + bankId + "/accounts/" + accountId + "/" + aPrivateViewId
        }
      }}
    }
    
    def loggedOutSnippet = {
      ".accountList" #> SHtml.span(Text("You don't have access to any authorised account"), Noop,("id","accountsMsg")) 
    }
    
    OBPUser.currentUser match {
      case Full(u) => loggedInSnippet(u)
      case _ => loggedOutSnippet
    }
  }
  
}