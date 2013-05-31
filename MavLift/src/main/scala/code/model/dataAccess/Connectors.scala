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
package code.model.dataAccess

import code.model._
import net.liftweb.common.{ Box, Empty, Full, Failure }
import net.liftweb.util.Helpers.tryo
import net.liftweb.mongodb.BsonDSL._
import net.liftweb.json.JsonDSL._
import net.liftweb.common.Loggable
import code.model.dataAccess.OBPEnvelope.OBPQueryParam
import net.liftweb.mapper.{By,IHaveValidatedThisSQL}
import net.liftweb.mongodb.MongoDB
import com.mongodb.BasicDBList
import java.util.ArrayList
import org.bson.types.ObjectId

/**
 *  this object will be used by the 'hight level' classes to get the data
 *  from any connector. The application allow to use just one connector at the time.
 *  Currently it is MongoDB.
 */

object LocalStorage extends MongoDBLocalStorage

trait LocalStorage extends Loggable {
  def getModeratedTransactions(permalink: String, bankPermalink: String)
  	(moderate: Transaction => ModeratedTransaction): List[ModeratedTransaction] = {
    val rawTransactions = getTransactions(permalink, bankPermalink) getOrElse Nil
    rawTransactions.map(moderate)
  }

  def getModeratedTransactions(permalink: String, bankPermalink: String, queryParams: OBPQueryParam*)
  	(moderate: Transaction => ModeratedTransaction): List[ModeratedTransaction] = {
    val rawTransactions = getTransactions(permalink, bankPermalink, queryParams: _*) getOrElse Nil
    rawTransactions.map(moderate)
  }

  def getTransactions(permalink: String, bankPermalink: String, queryParams: OBPQueryParam*) : Box[List[Transaction]] = {
    val envelopesForAccount = (acc: Account) => acc.envelopes(queryParams: _*)
    getTransactions(permalink, bankPermalink, envelopesForAccount)
  }

  def getTransactions(permalink: String, bankPermalink: String): Box[List[Transaction]] = {
    val envelopesForAccount = (acc: Account) => acc.allEnvelopes
    getTransactions(permalink, bankPermalink, envelopesForAccount)
  }

  def getTransactions(bank: String, account: String, envelopesForAccount: Account => List[OBPEnvelope]): Box[List[Transaction]]

  def getBank(name: String): Box[Bank]

  def getBankAccounts(bank: Bank): List[BankAccount]
  def correctBankAndAccount(bank: String, account: String): Boolean

  def getAccount(bankpermalink: String, account: String): Box[Account]
  def getTransaction(id : String, bankPermalink : String, accountPermalink : String) : Box[Transaction]

  def getModeratedTransaction(id : String, bankPermalink : String, accountPermalink : String)
  (moderate: Transaction => ModeratedTransaction) : Box[ModeratedTransaction] = {
    getTransaction(id,bankPermalink,accountPermalink) match {
      case Full(transaction) => Full(moderate(transaction))
      case _ => {
        val errorMessage = {"transaction id: " + id + " with bankID: " + bankPermalink + " and accountID: " + accountPermalink + " not found" }
        Failure(errorMessage,Empty,Empty)
      }
    }
  }

  def getUser(id : String) : Box[User]
  def getCurrentUser : Box[User]
  def getOtherAccount(accountID : String, otherAccountID : String) : Box[OtherBankAccount]

  def getAllAccounts() : List[BankAccount]
  def getAllPublicAccounts() : List[BankAccount]
  def getPublicBankAccounts(bank : Bank) : List[BankAccount]
  def getNonPublicBankAccounts(user : User) :  List[BankAccount]
  def getNonPublicBankAccounts(user : User, bankID : String) :  List[BankAccount]
  def permissions(account : BankAccount) : Box[List[Permission]]
}

class MongoDBLocalStorage extends LocalStorage {
  private def createTransaction(env: OBPEnvelope, theAccount: Account): Transaction = {
    import net.liftweb.json.JsonDSL._
    val transaction: OBPTransaction = env.obp_transaction.get
    val thisAccount = transaction.this_account
    val otherAccount_ = transaction.other_account.get
    val otherUnmediatedHolder = otherAccount_.holder.get

    val thisBankAccount = Account.toBankAccount(theAccount)

    val oAcc = theAccount.otherAccounts.objs.find(o => {
      otherUnmediatedHolder.equals(o.holder.get)
    }).getOrElse {
      OtherAccount.createRecord
    }

    val id = env.id.is.toString()
    val uuid = id
    val otherAccountMetadata =
      new OtherBankAccountMetadata(
        publicAlias = oAcc.publicAlias.get,
        privateAlias = oAcc.privateAlias.get,
        moreInfo = oAcc.moreInfo.get,
        url = oAcc.url.get,
        imageURL = oAcc.imageUrl.get,
        openCorporatesURL = oAcc.openCorporatesUrl.get,
        corporateLocations = oAcc.corporateLocation.get,
        physicalLocations = oAcc.physicalLocation.get,
        addMoreInfo = (text => {
          oAcc.moreInfo(text).save
          //the save method does not return a Boolean to inform about the saving state,
          //so we a true
          true
        }),
        addURL = (text => {
          oAcc.url(text).save
          //the save method does not return a Boolean to inform about the saving state,
          //so we a true
          true
        }),
        addImageURL = (text => {
          oAcc.imageUrl(text).save
          //the save method does not return a Boolean to inform about the saving state,
          //so we a true
          true
        }),
        addOpenCorporatesURL = (text => {
          oAcc.openCorporatesUrl(text).save
          //the save method does not return a Boolean to inform about the saving state,
          //so we a true
          true
        }),
        addCorporateLocation = oAcc.addCorporateLocation,
        addPhysicalLocation = oAcc.addPhysicalLocation,
        addPublicAlias = (alias => {
          oAcc.publicAlias(alias).save
          //the save method does not return a Boolean to inform about the saving state,
          //so we a true
          true
        }),
        addPrivateAlias = (alias => {
          oAcc.privateAlias(alias).save
          //the save method does not return a Boolean to inform about the saving state,
          //so we a true
          true
        }),
        deleteCorporateLocation = oAcc.deleteCorporateLocation,
        deletePhysicalLocation = oAcc.deletePhysicalLocation
      )
    val otherAccount = new OtherBankAccount(
        id = oAcc.id.is.toString,
        label = otherAccount_.holder.get,
        nationalIdentifier = otherAccount_.bank.get.national_identifier.get,
        swift_bic = None, //TODO: need to add this to the json/model
        iban = Some(otherAccount_.bank.get.IBAN.get),
        number = otherAccount_.number.get,
        bankName = otherAccount_.bank.get.name.get,
        metadata = otherAccountMetadata,
        kind = ""
      )
    val metadata = new TransactionMetadata(
      env.narrative.get,
      (text => env.narrative(text).save),
      env.obp_comments.objs,
      env.addComment,
      env.tags.objs,
      env.addTag,
      env.deleteTag,
      env.images.objs,
      env.addImage,
      env.deleteImage,
      env.whereTags.get,
      env.addWhereTag
    )
    val transactionType = transaction.details.get.type_en.get
    val amount = transaction.details.get.value.get.amount.get
    val currency = transaction.details.get.value.get.currency.get
    val label = Some(transaction.details.get.label.get)
    val startDate = transaction.details.get.posted.get
    val finishDate = transaction.details.get.completed.get
    val balance = transaction.details.get.new_balance.get.amount.get
    new Transaction(
      uuid,
      id,
      thisBankAccount,
      otherAccount,
      metadata,
      transactionType,
      amount,
      currency,
      label,
      startDate,
      finishDate,
      balance
    )
  }

  def getTransactions(permalink: String, bankPermalink: String, envelopesForAccount: Account => List[OBPEnvelope]): Box[List[Transaction]] = {
      logger.debug("getTransactions for " + bankPermalink + "/" + permalink)
      HostedBank.find("permalink",bankPermalink) match {
        case Full (bank) => bank.getAccount(permalink) match {
            case Full(account) => {
              val envs = envelopesForAccount(account)
              Full(envs.map(createTransaction(_, account)))
            }
            case _ => Empty
          }
        case _ => Empty
      }
  }

  def getBank(permalink: String): Box[Bank] =
    HostedBank.find("permalink", permalink) match {
      case Full(bank) =>
        Full(
          new Bank(
            bank.id.is.toString,
            bank.alias.is,
            bank.name.is,
            bank.permalink.is,
            bank.logoURL.is,
            bank.website.is
          )
        )
      case _ => Failure("bank " + permalink + " not found", Empty, Empty)
    }

  def allBanks : List[Bank] =
    HostedBank.findAll.
      map(
        bank =>
        new Bank(
          bank.id.is.toString,
          bank.alias.is,
          bank.name.is,
          bank.permalink.is,
          bank.logoURL.is,
          bank.website.is
        )
      )

  def getBankAccounts(bank: Bank): List[BankAccount] = {
    val bankId = new ObjectId(bank.id)
    val rawAccounts = Account.findAll(("bankID" -> bankId))
    rawAccounts.map(Account.toBankAccount)
  }

  def getPublicBankAccounts(bank : Bank) : List[BankAccount] = {
    val bankId = new ObjectId(bank.id)
    val rawAccounts = Account.findAll(("bankID",bankId) ~ ("anonAccess", true))
    rawAccounts.map(Account.toBankAccount)
  }

  /**
  * @return the bank accounts where the user has at least access to a non public view (is_public==false)
  */
  def getNonPublicBankAccounts(user : User) :  List[BankAccount] = {

    val hostedAccountTable = HostedAccount._dbTableNameLC
    val privilegeTable = Privilege._dbTableNameLC
    val userTable = OBPUser._dbTableNameLC

    val hostedId = hostedAccountTable + "." + HostedAccount.id.dbColumnName
    val hostedAccId = hostedAccountTable + "." + HostedAccount.accountID.dbColumnName
    val privilegeAccId = privilegeTable + "." + Privilege.account.dbColumnName
    val privilegeUserId = privilegeTable + "." + Privilege.user.dbColumnName
    val userId = user.id_

    val ourNetworkPrivilege = privilegeTable + "." + Privilege.ourNetworkPermission.dbColumnName
    val teamPrivilege = privilegeTable + "." + Privilege.teamPermission.dbColumnName
    val boardPrivilege = privilegeTable + "." + Privilege.boardPermission.dbColumnName
    val authoritiesPrivilege = privilegeTable + "." + Privilege.authoritiesPermission.dbColumnName
    val ownerPrivilege = privilegeTable + "." + Privilege.ownerPermission.dbColumnName
    val managementPrivilege = privilegeTable + "." + Privilege.mangementPermission.dbColumnName

    val query = "SELECT " + hostedId + ", " + hostedAccId +
          " FROM " + hostedAccountTable + ", " + privilegeTable + ", " + userTable +
          " WHERE " + "( " + hostedId + " = " + privilegeAccId + ")" +
            " AND " + "( " + privilegeUserId + " = " + userId + ")" +
            " AND " + "( " + ourNetworkPrivilege + " = true" +
              " OR " + teamPrivilege + " = true" +
              " OR " + boardPrivilege + " = true" +
              " OR " + authoritiesPrivilege + " = true" +
              " OR " + managementPrivilege + " = true" +
              " OR " + ownerPrivilege + " = true)"

    val moreThanAnon = HostedAccount.findAllByInsecureSql(query, IHaveValidatedThisSQL("everett", "nov. 15 2012"))
    val mongoIds = moreThanAnon.map(hAcc => new ObjectId(hAcc.accountID.get))

    Account.findAll(mongoIds).map(Account.toBankAccount)
  }
  def getNonPublicBankAccounts(user : User, bankID : String) :  List[BankAccount] = {

    val hostedAccountTable = HostedAccount._dbTableNameLC
    val privilegeTable = Privilege._dbTableNameLC
    val userTable = OBPUser._dbTableNameLC

    val hostedId = hostedAccountTable + "." + HostedAccount.id.dbColumnName
    val hostedAccId = hostedAccountTable + "." + HostedAccount.accountID.dbColumnName
    val privilegeAccId = privilegeTable + "." + Privilege.account.dbColumnName
    val privilegeUserId = privilegeTable + "." + Privilege.user.dbColumnName
    val userId = user.id_

    val ourNetworkPrivilege = privilegeTable + "." + Privilege.ourNetworkPermission.dbColumnName
    val teamPrivilege = privilegeTable + "." + Privilege.teamPermission.dbColumnName
    val boardPrivilege = privilegeTable + "." + Privilege.boardPermission.dbColumnName
    val authoritiesPrivilege = privilegeTable + "." + Privilege.authoritiesPermission.dbColumnName
    val ownerPrivilege = privilegeTable + "." + Privilege.ownerPermission.dbColumnName
    val managementPrivilege = privilegeTable + "." + Privilege.mangementPermission.dbColumnName

    val query = "SELECT " + hostedId + ", " + hostedAccId +
          " FROM " + hostedAccountTable + ", " + privilegeTable + ", " + userTable +
          " WHERE " + "( " + hostedId + " = " + privilegeAccId + ")" +
            " AND " + "( " + privilegeUserId + " = " + userId + ")" +
            " AND " + "( " + ourNetworkPrivilege + " = true" +
              " OR " + teamPrivilege + " = true" +
              " OR " + boardPrivilege + " = true" +
              " OR " + authoritiesPrivilege + " = true" +
              " OR " + managementPrivilege + " = true" +
              " OR " + ownerPrivilege + " = true)"

    val moreThanAnon = HostedAccount.findAllByInsecureSql(query, IHaveValidatedThisSQL("everett", "nov. 15 2012"))
    val mongoIds = moreThanAnon.map(hAcc => new ObjectId(hAcc.accountID.get))

    val bankObjectId = new ObjectId(bankID)
    def sameBank(account : Account) : Boolean =
      account.bankID.get == bankObjectId
    Account.findAll(mongoIds).filter(sameBank).map(Account.toBankAccount)
  }

  //check if the bank and the accounts exist in the database
  def correctBankAndAccount(bank: String, account: String): Boolean =
    HostedBank.find("permalink",bank) match {
        case Full(bank) => bank.isAccount(account)
        case _ => false
      }

  def getAccount(bankpermalink: String, account: String): Box[Account] =
    HostedBank.find("permalink",bankpermalink) match {
      case Full (bank) => bank.getAccount(account)
      case _ => Failure("Bank " + bankpermalink + "not found.", Empty, Empty)
    }

  def getTransaction(id : String, bankPermalink : String, accountPermalink : String) : Box[Transaction] = {
    for{
      bank <- HostedBank.find("permalink",bankPermalink)
      account  <- bank.getAccount(accountPermalink)
      objectId <- tryo{new ObjectId(id)} ?~ {"Transaction "+id+" not found"}
      ifTransactionsIsInAccount <- Full(account.transactionsForAccount.put("_id").is(objectId).get)
      envelope <- OBPEnvelope.find(ifTransactionsIsInAccount)
    } yield createTransaction(envelope,account)
  }

  def getAllAccounts() : List[BankAccount] = Account.findAll map Account.toBankAccount

  def getAllPublicAccounts() : List[BankAccount] = Account.findAll("anonAccess", true) map Account.toBankAccount

  def getUser(id : String) : Box[User] =
    tryo{
      id.toLong
    } match {
      case Full(idInLong) => {
        OBPUser.find(By(OBPUser.id,idInLong)) match {
          case Full(u) => Full(u)
          case _ => Failure("user " + id + " not found")
        }
      }
      case _ => Failure("user " + id + " not found")
    }

  def getCurrentUser : Box[User] = OBPUser.currentUser

  def getOtherAccount(accountID : String, otherAccountID : String) : Box[OtherBankAccount] = {
    Account.find("_id",new ObjectId(accountID)) match {
      case Full(account) =>
        account.otherAccounts.objs.find(otherAccount => { otherAccount.id.get.equals(otherAccountID)}) match {
          case Some(otherAccount) =>{
            //for legacy reasons some of the data about the "other account" are stored only on the transactions
            //so we need first to get a transaction that match to have the rest of the data
            val otherAccountFromTransaction : OBPAccount = OBPEnvelope.find("obp_transaction.other_account.holder",otherAccount.holder.get) match {
                case Full(envelope) =>
                  envelope.obp_transaction.get.other_account.get
                case _ => OBPAccount.createRecord
              }

            val metadata =
              new OtherBankAccountMetadata(
                publicAlias = otherAccount.publicAlias.get,
                privateAlias = otherAccount.privateAlias.get,
                moreInfo = otherAccount.moreInfo.get,
                url = otherAccount.url.get,
                imageURL = otherAccount.imageUrl.get,
                openCorporatesURL = otherAccount.openCorporatesUrl.get,
                corporateLocations = otherAccount.corporateLocation.get,
                physicalLocations = otherAccount.physicalLocation.get,
                addMoreInfo = (text => {
                  otherAccount.moreInfo(text).save
                  //the save method does not return a Boolean to inform about the saving state,
                  //so we a true
                  true
                }),
                addURL = (text => {
                  otherAccount.url(text).save
                  //the save method does not return a Boolean to inform about the saving state,
                  //so we a true
                  true
                }),
                addImageURL = (text => {
                  otherAccount.imageUrl(text).save
                  //the save method does not return a Boolean to inform about the saving state,
                  //so we a true
                  true
                }),
                addOpenCorporatesURL = (text => {
                  otherAccount.openCorporatesUrl(text).save
                  //the save method does not return a Boolean to inform about the saving state,
                  //so we a true
                  true
                }),
                addCorporateLocation = otherAccount.addCorporateLocation,
                addPhysicalLocation = otherAccount.addPhysicalLocation,
                addPublicAlias = (alias => {
                  otherAccount.publicAlias(alias).save
                  //the save method does not return a Boolean to inform about the saving state,
                  //so we a true
                  true
                }),
                addPrivateAlias = (alias => {
                  otherAccount.privateAlias(alias).save
                  //the save method does not return a Boolean to inform about the saving state,
                  //so we a true
                  true
                }),
                deleteCorporateLocation = otherAccount.deleteCorporateLocation,
                deletePhysicalLocation = otherAccount.deletePhysicalLocation
              )

            val otherBankAccount =
              new OtherBankAccount(
                id = otherAccount.id.is.toString,
                label = otherAccount.holder.get,
                nationalIdentifier = otherAccountFromTransaction.bank.get.national_identifier.get,
                swift_bic = None, //TODO: need to add this to the json/model
                iban = Some(otherAccountFromTransaction.bank.get.IBAN.get),
                number = otherAccountFromTransaction.number.get,
                bankName = otherAccountFromTransaction.bank.get.name.get,
                metadata = metadata,
                kind = ""
              )
              Full(otherBankAccount)
          }
          case _ => Failure("other account id " + otherAccountID + "not found", Empty, Empty)
        }
      case _ => Failure("Bank account id " + accountID + " not found.")
    }
  }
  def getOthersAccount(accountID : String) : Box[List[OtherBankAccount]] = {
    Account.find("_id",new ObjectId(accountID)) match {
      case Full(account) => {
        val otherBankAccounts = account.otherAccounts.objs.map(otherAccount => {
          //for legacy reasons some of the data about the "other account" are stored only on the transactions
          //so we need first to get a transaction that match to have the rest of the data
          val otherAccountFromTransaction : OBPAccount = OBPEnvelope.find("obp_transaction.other_account.holder",otherAccount.holder.get) match {
              case Full(envelope) =>
                envelope.obp_transaction.get.other_account.get
              case _ => OBPAccount.createRecord
            }

          val metadata =
            new OtherBankAccountMetadata(
              publicAlias = otherAccount.publicAlias.get,
              privateAlias = otherAccount.privateAlias.get,
              moreInfo = otherAccount.moreInfo.get,
              url = otherAccount.url.get,
              imageURL = otherAccount.imageUrl.get,
              openCorporatesURL = otherAccount.openCorporatesUrl.get,
              corporateLocations = otherAccount.corporateLocation.get,
              physicalLocations = otherAccount.physicalLocation.get,
              addMoreInfo = (text => {
                otherAccount.moreInfo(text).save
                //the save method does not return a Boolean to inform about the saving state,
                //so we a true
                true
              }),
              addURL = (text => {
                otherAccount.url(text).save
                //the save method does not return a Boolean to inform about the saving state,
                //so we a true
                true
              }),
              addImageURL = (text => {
                otherAccount.imageUrl(text).save
                //the save method does not return a Boolean to inform about the saving state,
                //so we a true
                true
              }),
              addOpenCorporatesURL = (text => {
                otherAccount.openCorporatesUrl(text).save
                //the save method does not return a Boolean to inform about the saving state,
                //so we a true
                true
              }),
              addCorporateLocation = otherAccount.addCorporateLocation _,
              addPhysicalLocation = otherAccount.addPhysicalLocation _,
              addPublicAlias = (alias => {
                otherAccount.publicAlias(alias).save
                //the save method does not return a Boolean to inform about the saving state,
                //so we a true
                true
              }),
              addPrivateAlias = (alias => {
                otherAccount.privateAlias(alias).save
                //the save method does not return a Boolean to inform about the saving state,
                //so we a true
                true
              }),
              deleteCorporateLocation = otherAccount.deleteCorporateLocation,
              deletePhysicalLocation = otherAccount.deletePhysicalLocation
            )

          val otherBankAccount =
            new OtherBankAccount(
              id = otherAccount.id.is.toString,
              label = otherAccount.holder.get,
              nationalIdentifier = otherAccountFromTransaction.bank.get.national_identifier.get,
              swift_bic = None, //TODO: need to add this to the json/model
              iban = Some(otherAccountFromTransaction.bank.get.IBAN.get),
              number = otherAccountFromTransaction.number.get,
              bankName = otherAccountFromTransaction.bank.get.name.get,
              metadata = metadata,
              kind = ""
            )
            otherBankAccount
        })
        Full(otherBankAccounts)
      }
      case _ => Failure("Bank account id " + accountID + " not found.")
    }
  }
  def permissions(account : BankAccount) : Box[List[Permission]] = {

    HostedAccount.find(By(HostedAccount.accountID,account.id)) match {
      case Full(acc) => {
        val privileges =  Privilege.findAll(By(Privilege.account, acc.id.get))
        val permissions : List[Box[Permission]] = privileges.map( p => {
            p.user.obj.map(u => {
                new Permission(
                  u,
                  u.permittedViews(account).toList
                )
              })
          })
        Full(permissions.flatten)
      }
      case _ => Failure("Could not find the hostedAccount", Empty, Empty)
    }
  }
  def addPermission(bankAccountId : String, view : View, user : User) : Box[Boolean] = {
    for{
      userId <- tryo{user.id_.toLong}
      bankAccount <- HostedAccount.find(By(HostedAccount.accountID, bankAccountId))
    } yield {
        val privilege = Privilege.create.
          user(userId).
          account(bankAccount)
        setPrivilegeFromView(privilege, view, true)
        privilege.save
      }
  }
  def revokePermission(bankAccountId : String, view : View, user : User) : Box[Boolean] = {
    for{
      userId <- tryo{user.id_.toLong}
      bankAccount <- HostedAccount.find(By(HostedAccount.accountID, bankAccountId))
    } yield {
        Privilege.find(By(Privilege.user, userId), By(Privilege.account, bankAccount)) match {
          case Full(privilege) => {
            setPrivilegeFromView(privilege, view, false)
            privilege.save
          }
          //there is no privilege to this user, so there is nothing to revoke
          case _ => true
        }
      }
  }
  private def setPrivilegeFromView(privilege : Privilege, view : View, value : Boolean ) = {
    view match {
      case OurNetwork => privilege.ourNetworkPermission(value)
      case Team => privilege.teamPermission(value)
      case Board => privilege.boardPermission(value)
      case Authorities => privilege.authoritiesPermission(value)
      case Owner => privilege.ownerPermission(value)
      case Management => privilege.mangementPermission(value)
    }
  }
}