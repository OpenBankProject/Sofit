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

package code.model.traits
import scala.math.BigDecimal
import java.util.Date
import code.model.dataAccess.LocalStorage
import net.liftweb.common.{Full, Empty, Failure, Box}
import code.model.dataAccess.Account
import code.model.dataAccess.OBPEnvelope.OBPQueryParam
import net.liftweb.json.JObject
import net.liftweb.json.JsonDSL._
import net.liftweb.http.LiftResponse
import net.liftweb.http.JsonResponse
import code.model.implementedTraits.View

trait BankAccount {

  def id : String

  var owners : Set[AccountOwner]

  //e.g. chequing, savings
  def accountType : String

  //TODO: Check if BigDecimal is an appropriate data type
  def balance : Option[BigDecimal]

  //ISO 4217, e.g. EUR, GBP, USD, etc.
  def currency: String

  def name : String

  //label to display, e.g. TESOBE Postbank Account
  def label : String

  def bankName : String

  def bankPermalink : String

  def permalink : String

  def number: String

  def nationalIdentifier : String

  def swift_bic : Option[String]

  def iban : Option[String]

  def transaction(id: String) : Box[Transaction]

  def moderatedTransaction(id: String, view: View, user: Box[User]) : Box[ModeratedTransaction] = {
    if(permittedViews(user).contains(view)) {
      transaction(id).map(view.moderate)
    } else Failure("view/transaction not authorized")
  }

  def moderatedBankAccount(view: View, user: Box[User]) : Box[ModeratedBankAccount] = {
    if(permittedViews(user).contains(view)){
      view.moderate(this) match {
        case Some(thisBankAccount) => Full(thisBankAccount)
        case _ => Failure("could not moderate this bank account id " + id, Empty, Empty)
      }
    }
    else
      Failure("user not allowed to access the " + view.name + " view.", Empty, Empty)
  }

  //Is a public view is available for this bank account
  def allowPublicAccess : Boolean

  def permittedViews(user: Box[User]) : Set[View]

  def getModeratedTransactions(moderate: Transaction => ModeratedTransaction): List[ModeratedTransaction]

  def getModeratedTransactions(queryParams: OBPQueryParam*)(moderate: Transaction => ModeratedTransaction): List[ModeratedTransaction]

  def authorizedAccess(view: View, user: Option[User]) : Boolean

  def overviewJson(user: Box[User]): JObject = {
    val views = permittedViews(user)
    ("number" -> number) ~
    ("account_alias" -> label) ~
    ("owner_description" -> "") ~
    ("views_available" -> views.map(view => view.toJson)) ~
    View.linksJson(views, permalink, bankPermalink)
  }

  def moderatedOtherBankAccount(otherAccountID : String, view : View, user : Box[User]) : Box[ModeratedOtherBankAccount] =
    if(permittedViews(user).contains(view)) {
      LocalStorage.getOtherAccount(id, otherAccountID) match {
        case Full(otherbankAccount) =>
          view.moderate(otherbankAccount) match {
            case Some(otherAccount) => Full(otherAccount)
            case _ => Failure("could not moderate the other account id " + otherAccountID)
          }
        case Failure(msg, _, _) => Failure(msg, Empty, Empty)
        case _ => Empty
      }
    }
    else
      Failure("user not allowed to access the " + view.name + " view.", Empty, Empty)
}

object BankAccount {
  def apply(bankpermalink: String, bankAccountPermalink: String) : Box[BankAccount] = {
    LocalStorage.getAccount(bankpermalink, bankAccountPermalink) match {
      case Full(account) => Full(Account.toBankAccount(account))
      case _ => Failure("account " + bankAccountPermalink +" not found in bank " + bankpermalink, Empty, Empty)
    }
  }

  def all : List[BankAccount] = {
    LocalStorage.getAllAccounts()
  }

  def publicAccounts : List[BankAccount] = {
    LocalStorage.getAllPublicAccounts()
  }

}