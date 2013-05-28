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
import code.snippet.CustomEditable
import net.liftweb.http.SHtml
import net.liftweb.json.JsonDSL._
import net.liftweb.json.JsonAST.JObject
import net.liftweb.common.Box
import net.liftweb.common.Empty
import net.liftweb.common.Full
import java.util.Date

class AliasType
class Alias extends AliasType
object PublicAlias extends Alias
object PrivateAlias extends Alias
object NoAlias extends AliasType
case class AccountName(display: String, aliasType: AliasType)
case class Permission(
  user : User,
  views : List[View]
)

trait View {

  //e.g. "Public", "Authorities", "Our Network", etc.
  def id: Long
  def name: String
  def description : String
  def permalink : String
  def isPublic : Boolean

  //the view settings
  def usePrivateAliasIfOneExists: Boolean
  def usePublicAliasIfOneExists: Boolean

  //reading access

  //transaction fields
  def canSeeTransactionThisBankAccount : Boolean
  def canSeeTransactionOtherBankAccount : Boolean
  def canSeeTransactionMetadata : Boolean
  def canSeeTransactionLabel: Boolean
  def canSeeTransactionAmount: Boolean
  def canSeeTransactionType: Boolean
  def canSeeTransactionCurrency: Boolean
  def canSeeTransactionStartDate: Boolean
  def canSeeTransactionFinishDate: Boolean
  def canSeeTransactionBalance: Boolean

  //transaction metadata
  def canSeeComments: Boolean
  def canSeeOwnerComment: Boolean
  def canSeeTags : Boolean
  def canSeeImages : Boolean

  //Bank account fields
  def canSeeBankAccountOwners : Boolean
  def canSeeBankAccountType : Boolean
  def canSeeBankAccountBalance : Boolean
  def canSeeBankAccountBalancePositiveOrNegative : Boolean
  def canSeeBankAccountCurrency : Boolean
  def canSeeBankAccountLabel : Boolean
  def canSeeBankAccountNationalIdentifier : Boolean
  def canSeeBankAccountSwift_bic : Boolean
  def canSeeBankAccountIban : Boolean
  def canSeeBankAccountNumber : Boolean
  def canSeeBankAccountBankName : Boolean
  def canSeeBankAccountBankPermalink : Boolean

  //other bank account fields
  def canSeeOtherAccountNationalIdentifier : Boolean
  def canSeeSWIFT_BIC : Boolean
  def canSeeOtherAccountIBAN : Boolean
  def canSeeOtherAccountBankName : Boolean
  def canSeeOtherAccountNumber : Boolean
  def canSeeOtherAccountMetadata : Boolean
  def canSeeOtherAccountKind : Boolean

  //other bank account meta data
  def canSeeMoreInfo: Boolean
  def canSeeUrl: Boolean
  def canSeeImageUrl: Boolean
  def canSeeOpenCorporatesUrl: Boolean
  def canSeeCorporateLocation : Boolean
  def canSeePhysicalLocation : Boolean
  def canSeePublicAlias : Boolean
  def canSeePrivateAlias : Boolean
  def canAddMoreInfo : Boolean
  def canAddURL : Boolean
  def canAddImageURL : Boolean
  def canAddOpenCorporatesUrl : Boolean
  def canAddCorporateLocation : Boolean
  def canAddPhysicalLocation : Boolean
  def canAddPublicAlias : Boolean
  def canAddPrivateAlias : Boolean
  def canDeleteCorporateLocation : Boolean

  //writing access
  def canEditOwnerComment: Boolean
  def canAddComments : Boolean
  def canAddTag : Boolean
  def canDeleteTag : Boolean
  def canAddImage : Boolean
  def canDeleteImage : Boolean
  def canAddWhereTag : Boolean
  def canSeeWhereTag : Boolean

  // In the future we can add a method here to allow someone to show only transactions over a certain limit

  def moderate(transaction: Transaction): ModeratedTransaction = {
    //transaction data
    val transactionId = transaction.id
    val transactionUUID = transaction.uuid
    val thisBankAccount = moderate(transaction.thisAccount)
    val otherBankAccount = moderate(transaction.otherAccount)

    //transation metadata
    val transactionMetadata =
      if(canSeeTransactionMetadata)
      {
        val ownerComment = if (canSeeOwnerComment) transaction.metadata.ownerComment else None
        val comments =
          if (canSeeComments)
            Some(transaction.metadata.comments.filter(comment => comment.viewId==id))
          else None
        val addCommentFunc= if(canAddComments) Some(transaction.metadata.addComment _) else None
        val addOwnerCommentFunc:Option[String=> Unit] = if (canEditOwnerComment) Some(transaction.metadata.ownerComment _) else None
        val tags =
          if(canSeeTags)
            Some(transaction.metadata.tags.filter(_.viewId==id))
          else None
        val addTagFunc =
          if(canAddTag)
            Some(transaction.metadata.addTag _)
          else
            None
        val deleteTagFunc =
            if(canDeleteTag)
              Some(transaction.metadata.deleteTag _)
            else
              None
        val images =
          if(canSeeImages) Some(transaction.metadata.images.filter(_.viewId == id))
          else None

        val addImageFunc =
          if(canAddImage) Some(transaction.metadata.addImage _)
          else None

        val deleteImageFunc =
          if(canDeleteImage) Some(transaction.metadata.deleteImage _)
          else None

        val addWhereTagFunc : Option[(String, Long, Date, Double, Double) => Boolean] =
          if(canAddWhereTag)
            Some(transaction.metadata.addWhereTag _)
          else
            Empty

        val whereTag =
          if(canSeeWhereTag)
            transaction.metadata.whereTags.find(tag => tag.viewId == id)
          else
            None

        new Some(
          new ModeratedTransactionMetadata(
            ownerComment,
            comments,
            addOwnerCommentFunc,
            addCommentFunc,
            tags,
            addTagFunc,
            deleteTagFunc,
            images,
            addImageFunc,
            deleteImageFunc,
            addWhereTagFunc,
            whereTag
        ))
      }
      else
        None

    val transactionType =
      if (canSeeTransactionType) Some(transaction.transactionType)
      else None

    val transactionAmount =
      if (canSeeTransactionAmount) Some(transaction.amount)
      else None

    val transactionCurrency =
      if (canSeeTransactionCurrency) Some(transaction.currency)
      else None

    val transactionLabel =
      if (canSeeTransactionLabel) transaction.label
      else None

    val transactionStartDate =
      if (canSeeTransactionStartDate) Some(transaction.startDate)
      else None

    val transactionFinishDate =
      if (canSeeTransactionFinishDate) Some(transaction.finishDate)
      else None

    val transactionBalance =
      if (canSeeTransactionBalance) transaction.balance.toString()
      else ""

    new ModeratedTransaction(transactionUUID, transactionId, thisBankAccount, otherBankAccount, transactionMetadata,
     transactionType, transactionAmount, transactionCurrency, transactionLabel, transactionStartDate,
      transactionFinishDate, transactionBalance)
  }

  def moderate(bankAccount: BankAccount) : Option[ModeratedBankAccount] = {
    if(canSeeTransactionThisBankAccount)
    {
      val owners : Set[AccountOwner] = if(canSeeBankAccountOwners) bankAccount.owners else Set()
      val balance =
        if(canSeeBankAccountBalance){
          bankAccount.balance.getOrElse(0).toString
        } else if(canSeeBankAccountBalancePositiveOrNegative) {
          if(bankAccount.balance.getOrElse(0).toString.startsWith("-")) "-" else "+"
        } else ""
      val accountType = if(canSeeBankAccountType) Some(bankAccount.accountType) else None
      val currency = if(canSeeBankAccountCurrency) Some(bankAccount.currency) else None
      val label = if(canSeeBankAccountLabel) Some(bankAccount.label) else None
      val nationalIdentifier = if(canSeeBankAccountNationalIdentifier) Some(bankAccount.label) else None
      val swiftBic = if(canSeeBankAccountSwift_bic) bankAccount.swift_bic else None
      val iban = if(canSeeBankAccountIban) bankAccount.iban else None
      val number = if(canSeeBankAccountNumber) Some(bankAccount.number) else None
      val bankName = if(canSeeBankAccountBankName) Some(bankAccount.bankName) else None
      val bankPermalink = if(canSeeBankAccountBankPermalink) Some(bankAccount.bankPermalink) else None

      Some(
        new ModeratedBankAccount(
          filteredId = bankAccount.permalink,
          filteredOwners = Some(owners),
          filteredAccountType = accountType,
          filteredBalance = balance,
          filteredCurrency = currency,
          filteredLabel = label,
          filteredNationalIdentifier = nationalIdentifier,
          filteredSwift_bic = swiftBic,
          filteredIban = iban,
          filteredNumber = number,
          filteredBankName = bankName,
          filteredBankPermalink = bankPermalink
        ))
    }
    else
      None
  }

  def moderate(otherBankAccount : OtherBankAccount) : Option[ModeratedOtherBankAccount] = {
    if (canSeeTransactionOtherBankAccount)
    {
      //other account data
      var otherAccountId = otherBankAccount.id
      val otherAccountLabel: AccountName = {
        val realName = otherBankAccount.label
        if (usePublicAliasIfOneExists) {

          val publicAlias = otherBankAccount.metadata.publicAlias

          if (! publicAlias.isEmpty ) AccountName(publicAlias, PublicAlias)
          else AccountName(realName, NoAlias)

        } else if (usePrivateAliasIfOneExists) {

          val privateAlias = otherBankAccount.metadata.privateAlias

          if (! privateAlias.isEmpty) AccountName(privateAlias, PrivateAlias)
          else AccountName(realName, PrivateAlias)
        } else
          AccountName(realName, NoAlias)
      }
      val otherAccountNationalIdentifier = if (canSeeOtherAccountNationalIdentifier) Some(otherBankAccount.nationalIdentifier) else None
      val otherAccountSWIFT_BIC = if (canSeeSWIFT_BIC) otherBankAccount.swift_bic else None
      val otherAccountIBAN = if(canSeeOtherAccountIBAN) otherBankAccount.iban else None
      val otherAccountBankName = if(canSeeOtherAccountBankName) Some(otherBankAccount.bankName) else None
      val otherAccountNumber = if(canSeeOtherAccountNumber) Some(otherBankAccount.number) else None
      val otherAccountKind = if(canSeeOtherAccountKind) Some(otherBankAccount.kind) else None
      val otherAccountMetadata =
        if(canSeeOtherAccountMetadata)
        {
          //other bank account metadata
          val moreInfo =
            if (canSeeMoreInfo) Some(otherBankAccount.metadata.moreInfo)
            else None
          val url =
            if (canSeeUrl) Some(otherBankAccount.metadata.url)
            else None
          val imageUrl =
            if (canSeeImageUrl) Some(otherBankAccount.metadata.imageUrl)
            else None
          val openCorporatesUrl =
            if (canSeeOpenCorporatesUrl) Some(otherBankAccount.metadata.openCorporatesUrl)
            else None
          val corporateLocation =
            if(canSeeCorporateLocation)
              otherBankAccount.metadata.corporateLocations.find(tag => tag.viewId == id)
            else
              None
          val physicalLocation =
            if(canSeePhysicalLocation)
              otherBankAccount.metadata.physicalLocations.find(tag => tag.viewId == id)
            else
              None
          val addMoreInfo =
            if(canAddMoreInfo)
              Some(otherBankAccount.metadata.addMoreInfo _)
            else
              None
          val addURL =
            if(canAddURL)
              Some(otherBankAccount.metadata.addURL _)
            else
              None
          val addImageURL =
            if(canAddImageURL)
              Some(otherBankAccount.metadata.addImageURL _)
            else
              None
          val addOpenCorporatesUrl =
            if(canAddOpenCorporatesUrl)
              Some(otherBankAccount.metadata.addOpenCorporatesUrl _)
            else
              None
          val addCorporateLocation =
            if(canAddCorporateLocation)
              Some(otherBankAccount.metadata.addCorporateLocation _)
            else
              None
          val addPhysicalLocation =
            if(canAddPhysicalLocation)
              Some(otherBankAccount.metadata.addPhysicalLocation _)
            else
              None
          val publicAlias =
            if(canSeePublicAlias)
              Some(otherBankAccount.metadata.publicAlias)
            else
              None
          val privateAlias =
            if(canSeePrivateAlias)
              Some(otherBankAccount.metadata.privateAlias)
            else
              None
          val addPublicAlias =
            if(canAddPublicAlias)
              Some(otherBankAccount.metadata.addPublicAlias _)
            else
              None
          val addPrivateAlias =
            if(canAddPrivateAlias)
              Some(otherBankAccount.metadata.addPrivateAlias _)
            else
              None
          val deleteCorporateLocation =
            if(canDeleteCorporateLocation)
              Some(otherBankAccount.metadata.deleteCorporateLocation _)
            else
              None


          Some(
            new ModeratedOtherBankAccountMetadata(
              moreInfo,
              url,
              imageUrl,
              openCorporatesUrl,
              corporateLocation,
              physicalLocation,
              publicAlias,
              privateAlias,
              addMoreInfo,
              addURL,
              addImageURL,
              addOpenCorporatesUrl,
              addCorporateLocation,
              addPhysicalLocation,
              addPublicAlias,
              addPrivateAlias,
              deleteCorporateLocation
          ))
        }
        else
            None

      Some(
        new ModeratedOtherBankAccount(
          otherAccountId,
          otherAccountLabel,
          otherAccountNationalIdentifier,
          otherAccountSWIFT_BIC,
          otherAccountIBAN,
          otherAccountBankName,
          otherAccountNumber,
          otherAccountMetadata,
          otherAccountKind))
    }
    else
      None
  }

  def toJson : JObject = {
    ("name" -> name) ~
    ("description" -> description)
  }

}

//An implementation that has the least amount of permissions possible
class BaseView extends View {
  def id = 1
  def name = "Restricted"
  def permalink = "restricted"
  def description = ""
  def isPublic = false

  //the view settings
  def usePrivateAliasIfOneExists = true
  def usePublicAliasIfOneExists = true

  //reading access

  //transaction fields
  def canSeeTransactionThisBankAccount = false
  def canSeeTransactionOtherBankAccount = false
  def canSeeTransactionMetadata = false
  def canSeeTransactionLabel = false
  def canSeeTransactionAmount = false
  def canSeeTransactionType = false
  def canSeeTransactionCurrency = false
  def canSeeTransactionStartDate = false
  def canSeeTransactionFinishDate = false
  def canSeeTransactionBalance = false

  //transaction metadata
  def canSeeComments = false
  def canSeeOwnerComment = false
  def canSeeTags = false
  def canSeeImages = false

  //Bank account fields
  def canSeeBankAccountOwners = false
  def canSeeBankAccountType = false
  def canSeeBankAccountBalance = false
  def canSeeBankAccountBalancePositiveOrNegative = false
  def canSeeBankAccountCurrency = false
  def canSeeBankAccountLabel = false
  def canSeeBankAccountNationalIdentifier = false
  def canSeeBankAccountSwift_bic = false
  def canSeeBankAccountIban = false
  def canSeeBankAccountNumber = false
  def canSeeBankAccountBankName = false
  def canSeeBankAccountBankPermalink = false

  //other bank account fields
  def canSeeOtherAccountNationalIdentifier = false
  def canSeeSWIFT_BIC = false
  def canSeeOtherAccountIBAN = false
  def canSeeOtherAccountBankName = false
  def canSeeOtherAccountNumber = false
  def canSeeOtherAccountMetadata = false
  def canSeeOtherAccountKind = false

  //other bank account meta data
  def canSeeMoreInfo = false
  def canSeeUrl = false
  def canSeeImageUrl = false
  def canSeeOpenCorporatesUrl = false
  def canSeeCorporateLocation = false
  def canSeePhysicalLocation = false
  def canSeePublicAlias = false
  def canSeePrivateAlias = false

  def canAddMoreInfo = false
  def canAddURL = false
  def canAddImageURL = false
  def canAddOpenCorporatesUrl = false
  def canAddCorporateLocation = false
  def canAddPhysicalLocation = false
  def canAddPublicAlias = false
  def canAddPrivateAlias = false
  def canDeleteCorporateLocation = false

  //writing access
  def canEditOwnerComment = false
  def canAddComments = false
  def canAddTag = false
  def canDeleteTag = false
  def canAddImage = false
  def canDeleteImage = false
  def canAddWhereTag = false
  def canSeeWhereTag = false
}

class FullView extends View {
  def id = 2
  def name = "Full"
  def permalink ="full"
  def description = ""
  def isPublic = false

  //the view settings
  def usePrivateAliasIfOneExists = false
  def usePublicAliasIfOneExists = false

  //reading access

  //transaction fields
  def canSeeTransactionThisBankAccount = true
  def canSeeTransactionOtherBankAccount = true
  def canSeeTransactionMetadata = true
  def canSeeTransactionLabel = true
  def canSeeTransactionAmount = true
  def canSeeTransactionType = true
  def canSeeTransactionCurrency = true
  def canSeeTransactionStartDate = true
  def canSeeTransactionFinishDate = true
  def canSeeTransactionBalance = true

  //transaction metadata
  def canSeeComments = true
  def canSeeOwnerComment = true
  def canSeeTags = true
  def canSeeImages = true

  //Bank account fields
  def canSeeBankAccountOwners = true
  def canSeeBankAccountType = true
  def canSeeBankAccountBalance = true
  def canSeeBankAccountBalancePositiveOrNegative = true
  def canSeeBankAccountCurrency = true
  def canSeeBankAccountLabel = true
  def canSeeBankAccountNationalIdentifier = true
  def canSeeBankAccountSwift_bic = true
  def canSeeBankAccountIban = true
  def canSeeBankAccountNumber = true
  def canSeeBankAccountBankName = true
  def canSeeBankAccountBankPermalink = true

  //other bank account fields
  def canSeeOtherAccountNationalIdentifier = true
  def canSeeSWIFT_BIC = true
  def canSeeOtherAccountIBAN = true
  def canSeeOtherAccountMetadata = true
  def canSeeOtherAccountBankName = true
  def canSeeOtherAccountNumber = true
  def canSeeOtherAccountKind = true

  //other bank account meta data
  def canSeeMoreInfo = true
  def canSeeUrl = true
  def canSeeImageUrl = true
  def canSeeOpenCorporatesUrl = true
  def canSeeCorporateLocation = true
  def canSeePhysicalLocation = true
  def canSeePublicAlias = true
  def canSeePrivateAlias = true

  def canAddMoreInfo = true
  def canAddURL = true
  def canAddImageURL = true
  def canAddOpenCorporatesUrl = true
  def canAddCorporateLocation = true
  def canAddPhysicalLocation = true
  def canAddPublicAlias = true
  def canAddPrivateAlias = true
  def canDeleteCorporateLocation = true

  //writing access
  def canEditOwnerComment = true
  def canAddComments = true
  def canAddTag = true
  def canDeleteTag = true
  def canAddImage = true
  def canDeleteImage = true
  def canAddWhereTag = true
  def canSeeWhereTag = true
}


