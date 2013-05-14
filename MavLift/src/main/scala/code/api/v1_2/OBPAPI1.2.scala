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
import code.model.implementedTraits.Owner


object OBPAPI1_2 extends OBPRestHelper with Loggable {

  implicit def errorToJson(error: ErrorMessage): JValue = Extraction.decompose(error)
  implicit def successToJson(success: SuccessMessage): JValue = Extraction.decompose(success)

  val dateFormat = ModeratedTransaction.dateFormat
  val apiPrefix = "obp" / "v1.2" oPrefix _

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

  private def moderatedTransactionMetadata(bankId : String, accountId : String, viewId : String, transactionID : String, user : Box[User]) : Box[ModeratedTransactionMetadata] =
    for {
      account <- BankAccount(bankId, accountId)
      view <- View.fromUrl(viewId)
      moderatedTransaction <- account.moderatedTransaction(transactionID, view, user)
      metadata <- Box(moderatedTransaction.metadata) ?~ {"view " + viewId + " does not authorize metadata access"}
    } yield metadata

  oauthServe(apiPrefix {
    case Nil JsonGet json => {
      user =>
        val apiDetails: JValue = {
          val hostedBy = new HostedBy("TESOBE", "contact@tesobe.com", "+49 (0)30 8145 3994")
          val apiInfoJSON = new APIInfoJSON("1.2", gitCommit, hostedBy)
          Extraction.decompose(apiInfoJSON)
        }

        Full(successJsonResponse(apiDetails, 200))
    }
  })

  oauthServe(apiPrefix {
    case "banks" :: Nil JsonGet json => {
      user =>
        def banksToJson(banksList: List[Bank]): JValue = {
          val banksJSON: List[BankJSON] = banksList.map(b => {
            JSONFactory.createBankJSON(b)
          })
          val banks = new BanksJSON(banksJSON)
          Extraction.decompose(banks)
        }

        Full(successJsonResponse(banksToJson(Bank.all)))
    }
  })

  oauthServe(apiPrefix{
    case "banks" :: bankId :: Nil JsonGet json => {
      user =>
        def bankToJson(bank : Bank) : JValue = {
          val bankJSON = JSONFactory.createBankJSON(bank)
          Extraction.decompose(bankJSON)
        }

        for(bank <- Bank(bankId))
          yield successJsonResponse(bankToJson(bank))
    }
  })

  oauthServe(apiPrefix {
    case "banks" :: bankId :: "accounts" :: Nil JsonGet json => {
      user =>
        for (bank <- Bank(bankId) )
          yield {
            val availableAccounts = user match {
              case Full(u) => bank.accounts.filter(_.permittedViews(user).size != 0)
              case _ => bank.publicAccounts
            }
            successJsonResponse(bankAccountsListToJson(availableAccounts, user))
          }
    }
  })

  oauthServe(apiPrefix {
    case "banks" :: bankId :: "accounts" :: "private" :: Nil JsonGet json => {
      user =>
        for {
          u <- user ?~ "user not found"
          bank <- Bank(bankId)
        } yield {
            val availableAccounts = bank.nonPublicAccounts(u)
            successJsonResponse(bankAccountsListToJson(availableAccounts, Full(u)))
          }
    }
  })

  oauthServe(apiPrefix {
    case "banks" :: bankId :: "accounts" :: "public" :: Nil JsonGet json => {
      user =>
        for {
          bank <- Bank(bankId)
        } yield {
          val availableAccounts = bank.publicAccounts
          val publicAccountsJson = bankAccountsListToJson(availableAccounts, user)
          successJsonResponse(publicAccountsJson)
        }
    }
  })

  oauthServe(apiPrefix {
    case "banks" :: bankId :: "accounts" :: accountId :: viewId :: "account" :: Nil JsonGet json => {
      user =>
        for {
          account <- BankAccount(bankId, accountId)
          availableviews <- Full(account.permittedViews(user))
          view <- View.fromUrl(viewId)
          moderatedAccount <- account.moderatedBankAccount(view, user)
        } yield {
            val viewsAvailable = availableviews.map(JSONFactory.createViewJSON)
            val moderatedAccountJson = JSONFactory.createModeratedAccountJSON(moderatedAccount, viewsAvailable)
            successJsonResponse(Extraction.decompose(moderatedAccountJson))
          }
    }
  })

  oauthServe(apiPrefix {
    case "banks" :: bankId :: "accounts" :: accountId :: "users" :: Nil JsonGet json => {
      user =>
        for {
          account <- BankAccount(bankId, accountId)
          u <- user ?~ "user not found"
          permissions <- account permissions u
        } yield {
            val permissionsJSON = JSONFactory.createPermissionsJSON(permissions)
            successJsonResponse(Extraction.decompose(permissionsJSON))
          }
    }
  })

  oauthServe(apiPrefix {
    case "banks" :: bankId :: "accounts" :: accountId :: "users" :: userId :: Nil JsonGet json => {
      user =>
        for {
          account <- BankAccount(bankId, accountId)
          u <- user ?~ "user not found"
          permissions <- account permissions u
          userPermission <- Box(permissions.find(p => { p.user.id_ == userId})) ?~ {userId +" not found" }
        } yield {
            val views = JSONFactory.createViewsJSON(userPermission.views)
            successJsonResponse(Extraction.decompose(views))
          }
    }
  })

  oauthServe(apiPrefix{
    case "banks" :: bankId :: "accounts" :: accountId :: "users" :: userId :: "views" :: viewId :: Nil JsonPost json => {
      user =>
        for {
          account <- BankAccount(bankId, accountId)
          u <- user ?~ "user not found"
          view <- View.fromUrl(viewId)
          isAdded <- account addPermission(u, viewId, userId)
          if(isAdded)
        } yield {
            val viewJson = JSONFactory.createViewJSON(view)
            successJsonResponse(Extraction.decompose(viewJson), 201)
          }
    }
  })

  oauthServe(apiPrefix{
    case "banks" :: bankId :: "accounts" :: accountId :: "users" :: userId :: "views" :: viewId :: Nil JsonDelete json => {
      user =>
        for {
          account <- BankAccount(bankId, accountId)
          u <- user ?~ "user not found"
          isRevoked <- account revokePermission(u, viewId, userId)
          if(isRevoked)
        } yield noContentJsonResponse
    }
  })

  oauthServe(apiPrefix {
    case "banks" :: bankId :: "accounts" :: accountId :: viewId :: "transactions" :: transactionId :: "transaction" :: Nil JsonGet json => {
      user =>
        for {
          account <- BankAccount(bankId, accountId)
          view <- View.fromUrl(viewId)
          moderatedTransaction <- account.moderatedTransaction(transactionId, view, user) ?~ "view/transaction not authorized"
        } yield {
            val json = JSONFactory.createTransactionJSON(moderatedTransaction)
            successJsonResponse(Extraction.decompose(json))
          }
    }
  })

  oauthServe(apiPrefix {
    case "banks" :: bankId :: "accounts" :: accountId :: viewId :: "transactions" :: transactionId :: "metadata" :: "comments" :: Nil JsonGet json => {
      user =>
        for {
          metadata <- moderatedTransactionMetadata(bankId, accountId, viewId, transactionId, user)
          comments <- Box(metadata.comments) ?~ { "view " + viewId + " does not authorize comments access" }
        } yield {
          val json = JSONFactory.createTransactionCommentsJson(comments)
          successJsonResponse(Extraction.decompose(json))
        }
    }
  })

  oauthServe(apiPrefix {
    case "banks" :: bankId :: "accounts" :: accountId :: viewId :: "transactions" :: transactionID :: "metadata" :: "tags" :: Nil JsonPost json -> _ => {
      user =>
        for {
          tagJson <- tryo{json.extract[PostTransactionTagJSON]} ?~ {"Wrong format of the posted JSON"}
          u <- user ?~ "user not found"
          view <- View.fromUrl(viewId)
          metadata <- moderatedTransactionMetadata(bankId, accountId, view.permalink, transactionID, Full(u))
          addTagFunc <- if(view.canAddTag) Box(metadata.addTag) ?~ {"view " + viewId + " does not authorize adding tags"}
                          else Failure("view does not allow tags to be added")
          postedTagId <- Full(addTagFunc(u.id_, view.id, tagJson.value, now))
          newMetadata <- moderatedTransactionMetadata(bankId, accountId, view.permalink, transactionID, Full(u))
          allTags <- Box(newMetadata.tags) ?~! "Server error: no tags found after posting"
          postedTag <- Box(allTags.find(tag => tag.id_ == postedTagId)) ?~! "Server error: posted tag not found after posting"
        } yield {
          successJsonResponse(Extraction.decompose(JSONFactory.createTransactionTagJSON(postedTag)))
        }
    }
  })

  oauthServe(apiPrefix {
    case "banks" :: bankId :: "accounts" :: accountId :: viewId :: "transactions" :: transactionId :: "metadata" :: "tags" :: tagId :: Nil JsonDelete _ => {
      user =>
        for {
          metadata <- moderatedTransactionMetadata(bankId, accountId, viewId, transactionId, user)
          tags <- Box(metadata.tags) ?~ { "view " + viewId + " does not authorize tags access" }
          toDelete <- Box(tags.find(tags => tags.id_ == tagId)) ?~ { "tag not found" }
          view <- View.fromUrl(viewId)
          bankAccount <- BankAccount(bankId, accountId)
          deletable <- if (toDelete.postedBy == user || bankAccount.permittedViews(user).contains(Owner)) Full(toDelete)
          else Failure("insufficient privileges to delete tag")
          deleteFunction <- if (view.canDeleteTag) Box(metadata.deleteTag)
          else Failure("view does not allow tags to be deleted")
        } yield {
            deleteFunction(deletable.id_)
            noContentJsonResponse
          }
    }
  })
  oauthServe(apiPrefix {
    case "banks" :: bankId :: "accounts" :: accountId :: viewId :: "transactions" :: transactionId :: "metadata" :: "images" :: Nil JsonGet json => {
      user =>
        for {
          metadata <- moderatedTransactionMetadata(bankId, accountId, viewId, transactionId, user)
          images <- Box(metadata.images) ?~ { "view " + viewId + " does not authorize images access" }
        } yield {
          val json = JSONFactory.createTransactionImagesJson(images)
          successJsonResponse(Extraction.decompose(json))
        }
    }
  })

  oauthServe(apiPrefix {
    case "banks" :: bankId :: "accounts" :: accountId :: viewId :: "transactions" :: transactionID :: "metadata" :: "images" :: Nil JsonPost json -> _ => {
      user =>
        for {
          imageJson <- tryo{json.extract[PostTransactionImageJSON]}
          u <- user
          view <- View.fromUrl(viewId)
          metadata <- moderatedTransactionMetadata(bankId, accountId, view.permalink, transactionID, Full(u))
          addImageFunc <- if(view.canAddImage) Box(metadata.addImage) ?~ {"view " + viewId + " does not authorize adding comment"}
                          else Failure("view does not allow images to be added")
          url <- tryo{new URL(imageJson.URL)} ?~! "Could not parse url string as a valid URL"
          postedImageId <- Full(addImageFunc(u.id_, view.id, imageJson.label, now, url))
          newMetadata <- moderatedTransactionMetadata(bankId, accountId, view.permalink, transactionID, Full(u))
          allImages <- Box(newMetadata.images) ?~! "Server error: no images found after posting"
          postedImage <- Box(allImages.find(image => image.id_ == postedImageId)) ?~! "Server error: posted image not found after posting"
        } yield {
          successJsonResponse(Extraction.decompose(JSONFactory.createTransactionImageJSON(postedImage)))
        }
    }
  })


  oauthServe(apiPrefix {
    case "banks" :: bankId :: "accounts" :: accountId :: viewId :: "transactions" :: transactionId :: "metadata" :: "images" :: imageId :: Nil JsonDelete _ => {
      user =>
        for {
          metadata <- moderatedTransactionMetadata(bankId, accountId, viewId, transactionId, user)
          images <- Box(metadata.images) ?~ { "view " + viewId + " does not authorize image access" }
          toDelete <- Box(images.find(image => image.id_ == imageId)) ?~ { "image not found" }
          view <- View.fromUrl(viewId)
          bankAccount <- BankAccount(bankId, accountId)
          deletable <- if(toDelete.postedBy == user || bankAccount.permittedViews(user).contains(Owner)) Full(toDelete)
                       else Failure("insufficient privileges to delete image")
          deleteFunction <- if(view.canDeleteImage) Box(metadata.deleteImage)
                            else Failure("view does not allow images to be deleted")
        } yield {
          deleteFunction(deletable.id_)
          noContentJsonResponse
        }
    }
  })
}