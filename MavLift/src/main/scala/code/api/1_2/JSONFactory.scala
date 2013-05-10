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

import java.util.Date
import net.liftweb.common.{Box, Full}
import code.model.traits._

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
case class MinimalBankJSON(
  national_identifier : String,
  name : String
)
case class BankJSON(
  id : String,
  short_name : String,
  full_name : String,
  logo : String,
  website : String
)
case class ViewsJSON(
  views : List[ViewJSON]
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
  owners : List[UserJSON],
  `type` : String,
  balance : AmountOfMoneyJSON,
  IBAN : String,
  views_available : Set[ViewJSON],
  bank_id : String
)
case class UserJSON(
  id : String,
  provider : String,
  display_name : String
)
case class PermissionsJSON(
  permissions : List[PermissionJSON]
)
case class PermissionJSON(
  user : UserJSON,
  views : List[ViewJSON]
)
case class AmountOfMoneyJSON(
  currency : String,
  amount : String
)
case class AccountHolderJSON(
  name : String,
  is_alias : Boolean
)
case class ThisAccountJSON(
  holders : List[AccountHolderJSON],
  number : String,
  kind : String,
  IBAN : String,
  bank : MinimalBankJSON
)
case class OtherAccountJSON(
  holder : AccountHolderJSON,
  number : String,
  kind : String,
  IBAN : String,
  bank : MinimalBankJSON,
  metadata : OtherAccountMetadataJSON)
case class OtherAccountMetadataJSON(
  public_alias : String,
  private_alias : String,
  more_info : String,
  URL : String,
  image_URL : String,
  open_corporates_URL : String,
  corporate_location : LocationJSON,
  physical_location : LocationJSON
)
case class LocationJSON(
  latitude : Double,
  longitude : Double,
  date : Date,
  user : UserJSON
)
case class TransactionDetailsJSON(
  `type` : String,
  label : String,
  posted : Date,
  completed : Date,
  new_balance : AmountOfMoneyJSON,
  value : AmountOfMoneyJSON
)
case class TransactionMetadataJSON(
  narrative : String,
  comments : List[TransactionCommentJSON],
  tags : List[TransactionTagJSON],
  images : List[TransactionImageJSON],
  where : LocationJSON
)
case class TransactionJSON(
  id : String,
  this_account : ThisAccountJSON,
  other_account : OtherAccountJSON,
  details : TransactionDetailsJSON,
  metadata : TransactionMetadataJSON)
case class TransactionImagesJSON(
  images : List[TransactionImageJSON])
case class TransactionImageJSON(
  id : String,
  label : String,
  URL : String,
  date : Date,
  user : UserJSON)
case class TransactionTagJSON(
  id : String,
  value : String,
  date : Date,
  user : UserJSON)
case class TransactionCommentJSON(
  id : String,
  value : String,
  date: Date,
  user : UserJSON
)

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

  def createViewsJSON(views : List[View]) : ViewsJSON = {
    val list : List[ViewJSON] = views.map(createViewJSON)
    new ViewsJSON(list)
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

  def createTransactionJson(transaction : ModeratedTransaction) : TransactionJSON = {
    new TransactionJSON(
        id = transaction.id,
        this_account = transaction.bankAccount.map(createThisAccountJSON).getOrElse(null),
        other_account = transaction.otherBankAccount.map(createOtherAccountJSON).getOrElse(null),
        details = createTransactionDetailsJSON(transaction),
        metadata = createTransactionMetadataJSON(transaction)
      )
  }

  def createTransactionMetadataJSON(transaction : ModeratedTransaction) : TransactionMetadataJSON = {
    //TODO
    null
  }

  def createTransactionDetailsJSON(transaction : ModeratedTransaction) : TransactionDetailsJSON = {
    //TODO
    null
  }

  def createThisAccountJSON(bankAccount : ModeratedBankAccount) : ThisAccountJSON = {
    //TODO
    null
  }

  def createOtherAccountJSON(bankAccount : ModeratedOtherBankAccount) : OtherAccountJSON = {
    //TODO
    null
  }

  def createUserJSON(user : User) : UserJSON = {
    new UserJSON(
          user.id_,
          stringOrNull(user.provider),
          stringOrNull(user.emailAddress)
        )
  }

  def createUserJSON(user : Box[User]) : UserJSON = {
    user match {
      case Full(u) => createUserJSON(u)
      case _ => null
    }
  }

  def createOwnersJSON(owners : Set[AccountOwner], bankName : String) : List[UserJSON] = {
    owners.map(o => {
        new UserJSON(
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
  def createPermissionsJSON(permissions : List[Permission]) : PermissionsJSON = {
    val permissionsJson = permissions.map(p => {
        new PermissionJSON(
          createUserJSON(p.user),
          p.views.map(createViewJSON)
        )
      })
    new PermissionsJSON(permissionsJson)
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