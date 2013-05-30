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
import java.util.Date
import code.api.OAuthHandshake._
import code.model.dataAccess.OBPEnvelope.{OBPOrder, OBPLimit, OBPOffset, OBPOrdering, OBPFromDate, OBPToDate, OBPQueryParam}
import code.model._
import java.net.URL
import code.util.APIUtil._
import code.api.OBPRestHelper


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
            val moderatedAccountJson = JSONFactory.createBankAccountJSON(moderatedAccount, viewsAvailable)
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

  oauthServe(apiPrefix{
    case "banks" :: bankId :: "accounts" :: accountId :: viewId :: "other_accounts" :: Nil JsonGet json => {
      user =>
        for {
          account <- BankAccount(bankId, accountId)
          view <- View.fromUrl(viewId)
          otherBankAccounts <- account.moderatedOtherBankAccounts(view, user)
        } yield {
          val otherBankAccountsJson = JSONFactory.createOtherBankAccountsJSON(otherBankAccounts)
          successJsonResponse(Extraction.decompose(otherBankAccountsJson))
        }
    }
  })

  oauthServe(apiPrefix{
    case "banks" :: bankId :: "accounts" :: accountId :: viewId :: "other_accounts":: other_account_id :: Nil JsonGet json => {
      user =>
        for {
          account <- BankAccount(bankId, accountId)
          view <- View.fromUrl(viewId)
          otherBankAccount <- account.moderatedOtherBankAccount(other_account_id, view, user)
        } yield {
          val otherBankAccountJson = JSONFactory.createOtherBankAccount(otherBankAccount)
          successJsonResponse(Extraction.decompose(otherBankAccountJson))
        }
    }
  })

  oauthServe(apiPrefix{
    case "banks" :: bankId :: "accounts" :: accountId :: viewId :: "other_accounts":: other_account_id :: "metadata" :: Nil JsonGet json => {
      user =>
        for {
          account <- BankAccount(bankId, accountId)
          view <- View.fromUrl(viewId)
          otherBankAccount <- account.moderatedOtherBankAccount(other_account_id, view, user)
          metadata <- Box(otherBankAccount.metadata) ?~ {"the view " + viewId + "does not allow metadata access"}
        } yield {
          val metadataJson = JSONFactory.createOtherAccountMetaDataJSON(metadata)
          successJsonResponse(Extraction.decompose(metadataJson))
        }
    }
  })

  oauthServe(apiPrefix{
    case "banks" :: bankId :: "accounts" :: accountId :: viewId :: "other_accounts":: other_account_id :: "public_alias" :: Nil JsonGet json => {
      user =>
        for {
          account <- BankAccount(bankId, accountId)
          view <- View.fromUrl(viewId)
          otherBankAccount <- account.moderatedOtherBankAccount(other_account_id, view, user)
          metadata <- Box(otherBankAccount.metadata) ?~ {"the view " + viewId + "does not allow metadata access"}
          alias <- Box(metadata.publicAlias) ?~ {"the view " + viewId + "does not allow public alias access"}
        } yield {
          val aliasJson = JSONFactory.createAliasJSON(alias)
          successJsonResponse(Extraction.decompose(aliasJson))
        }
    }
  })

  oauthServe(apiPrefix{
    case "banks" :: bankId :: "accounts" :: accountId :: viewId :: "other_accounts":: other_account_id :: "public_alias" :: Nil JsonPost json -> _ => {
      user =>
        for {
          account <- BankAccount(bankId, accountId)
          view <- View.fromUrl(viewId)
          otherBankAccount <- account.moderatedOtherBankAccount(other_account_id, view, user)
          metadata <- Box(otherBankAccount.metadata) ?~ {"the view " + viewId + "does not allow metadata access"}
          addAlias <- Box(metadata.addPublicAlias) ?~ {"the view " + viewId + "does not allow adding a public alias"}
          aliasJson <- tryo{(json.extract[AliasJSON])} ?~ {"wrong JSON format"}
          if(addAlias(aliasJson.alias))
        } yield {
            successJsonResponse(Extraction.decompose(SuccessMessage("public alias added")), 201)
        }
    }
  })

  oauthServe(apiPrefix{
    case "banks" :: bankId :: "accounts" :: accountId :: viewId :: "other_accounts":: other_account_id :: "public_alias" :: Nil JsonPut json -> _ => {
      user =>
        for {
          account <- BankAccount(bankId, accountId)
          view <- View.fromUrl(viewId)
          otherBankAccount <- account.moderatedOtherBankAccount(other_account_id, view, user)
          metadata <- Box(otherBankAccount.metadata) ?~ {"the view " + viewId + "does not allow metadata access"}
          addAlias <- Box(metadata.addPublicAlias) ?~ {"the view " + viewId + "does not allow updating the public alias"}
          aliasJson <- tryo{(json.extract[AliasJSON])} ?~ {"wrong JSON format"}
          if(addAlias(aliasJson.alias))
        } yield {
            successJsonResponse(Extraction.decompose(SuccessMessage("public alias updated")))
        }
    }
  })

  oauthServe(apiPrefix{
    case "banks" :: bankId :: "accounts" :: accountId :: viewId :: "other_accounts":: other_account_id :: "public_alias" :: Nil JsonDelete _ => {
      user =>
        for {
          account <- BankAccount(bankId, accountId)
          view <- View.fromUrl(viewId)
          otherBankAccount <- account.moderatedOtherBankAccount(other_account_id, view, user)
          metadata <- Box(otherBankAccount.metadata) ?~ {"the view " + viewId + "does not allow metadata access"}
          addAlias <- Box(metadata.addPublicAlias) ?~ {"the view " + viewId + "does not allow deleting the public alias"}
          if(addAlias(""))
        } yield noContentJsonResponse
    }
  })


  oauthServe(apiPrefix{
    case "banks" :: bankId :: "accounts" :: accountId :: viewId :: "other_accounts":: other_account_id :: "private_alias" :: Nil JsonGet json => {
      user =>
        for {
          account <- BankAccount(bankId, accountId)
          view <- View.fromUrl(viewId)
          otherBankAccount <- account.moderatedOtherBankAccount(other_account_id, view, user)
          metadata <- Box(otherBankAccount.metadata) ?~ {"the view " + viewId + "does not allow metadata access"}
          alias <- Box(metadata.privateAlias) ?~ {"the view " + viewId + "does not allow private alias access"}
        } yield {
          val aliasJson = JSONFactory.createAliasJSON(alias)
          successJsonResponse(Extraction.decompose(aliasJson))
        }
    }
  })

  oauthServe(apiPrefix{
    case "banks" :: bankId :: "accounts" :: accountId :: viewId :: "other_accounts":: other_account_id :: "private_alias" :: Nil JsonPost json -> _ => {
      user =>
        for {
          account <- BankAccount(bankId, accountId)
          view <- View.fromUrl(viewId)
          otherBankAccount <- account.moderatedOtherBankAccount(other_account_id, view, user)
          metadata <- Box(otherBankAccount.metadata) ?~ {"the view " + viewId + "does not allow metadata access"}
          addAlias <- Box(metadata.addPrivateAlias) ?~ {"the view " + viewId + "does not allow adding a private alias"}
          aliasJson <- tryo{(json.extract[AliasJSON])} ?~ {"wrong JSON format"}
          if(addAlias(aliasJson.alias))
        } yield {
            val successJson = SuccessMessage("private alias added")
            successJsonResponse(Extraction.decompose(successJson), 201)
        }
    }
  })

  oauthServe(apiPrefix{
    case "banks" :: bankId :: "accounts" :: accountId :: viewId :: "other_accounts":: other_account_id :: "private_alias" :: Nil JsonPut json -> _ => {
      user =>
        for {
          account <- BankAccount(bankId, accountId)
          view <- View.fromUrl(viewId)
          otherBankAccount <- account.moderatedOtherBankAccount(other_account_id, view, user)
          metadata <- Box(otherBankAccount.metadata) ?~ {"the view " + viewId + "does not allow metadata access"}
          addAlias <- Box(metadata.addPrivateAlias) ?~ {"the view " + viewId + "does not allow updating the private alias"}
          aliasJson <- tryo{(json.extract[AliasJSON])} ?~ {"wrong JSON format"}
          if(addAlias(aliasJson.alias))
        } yield {
            val successJson = SuccessMessage("private alias updated")
            successJsonResponse(Extraction.decompose(successJson))
        }
    }
  })

  oauthServe(apiPrefix{
    case "banks" :: bankId :: "accounts" :: accountId :: viewId :: "other_accounts":: other_account_id :: "private_alias" :: Nil JsonDelete _ => {
      user =>
        for {
          account <- BankAccount(bankId, accountId)
          view <- View.fromUrl(viewId)
          otherBankAccount <- account.moderatedOtherBankAccount(other_account_id, view, user)
          metadata <- Box(otherBankAccount.metadata) ?~ {"the view " + viewId + "does not allow metadata access"}
          addAlias <- Box(metadata.addPrivateAlias) ?~ {"the view " + viewId + "does not allow deleting the private alias"}
          if(addAlias(""))
        } yield noContentJsonResponse
    }
  })

  oauthServe(apiPrefix{
    case "banks" :: bankId :: "accounts" :: accountId :: viewId :: "other_accounts":: other_account_id :: "more_info" :: Nil JsonPost json -> _ => {
      user =>
        for {
          account <- BankAccount(bankId, accountId)
          view <- View.fromUrl(viewId)
          otherBankAccount <- account.moderatedOtherBankAccount(other_account_id, view, user)
          metadata <- Box(otherBankAccount.metadata) ?~ {"the view " + viewId + "does not allow metadata access"}
          addMoreInfo <- Box(metadata.addMoreInfo) ?~ {"the view " + viewId + "does not allow adding more info"}
          moreInfoJson <- tryo{(json.extract[MoreInfoJSON])} ?~ {"wrong JSON format"}
          if(addMoreInfo(moreInfoJson.more_info))
        } yield {
            val successJson = SuccessMessage("more info added")
            successJsonResponse(Extraction.decompose(successJson), 201)
        }
    }
  })

  oauthServe(apiPrefix{
    case "banks" :: bankId :: "accounts" :: accountId :: viewId :: "other_accounts":: other_account_id :: "more_info" :: Nil JsonPut json -> _ => {
      user =>
        for {
          account <- BankAccount(bankId, accountId)
          view <- View.fromUrl(viewId)
          otherBankAccount <- account.moderatedOtherBankAccount(other_account_id, view, user)
          metadata <- Box(otherBankAccount.metadata) ?~ {"the view " + viewId + "does not allow metadata access"}
          addMoreInfo <- Box(metadata.addMoreInfo) ?~ {"the view " + viewId + "does not allow updating more info"}
          moreInfoJson <- tryo{(json.extract[MoreInfoJSON])} ?~ {"wrong JSON format"}
          if(addMoreInfo(moreInfoJson.more_info))
        } yield {
            val successJson = SuccessMessage("more info updated")
            successJsonResponse(Extraction.decompose(successJson))
        }
    }
  })

  oauthServe(apiPrefix{
    case "banks" :: bankId :: "accounts" :: accountId :: viewId :: "other_accounts":: other_account_id :: "more_info" :: Nil JsonDelete _ => {
      user =>
        for {
          account <- BankAccount(bankId, accountId)
          view <- View.fromUrl(viewId)
          otherBankAccount <- account.moderatedOtherBankAccount(other_account_id, view, user)
          metadata <- Box(otherBankAccount.metadata) ?~ {"the view " + viewId + "does not allow metadata access"}
          addMoreInfo <- Box(metadata.addMoreInfo) ?~ {"the view " + viewId + "does not allow deleting more info"}
          if(addMoreInfo(""))
        } yield noContentJsonResponse
    }
  })

  oauthServe(apiPrefix{
    case "banks" :: bankId :: "accounts" :: accountId :: viewId :: "other_accounts":: other_account_id :: "url" :: Nil JsonPost json -> _ => {
      user =>
        for {
          account <- BankAccount(bankId, accountId)
          view <- View.fromUrl(viewId)
          otherBankAccount <- account.moderatedOtherBankAccount(other_account_id, view, user)
          metadata <- Box(otherBankAccount.metadata) ?~ {"the view " + viewId + "does not allow metadata access"}
          addUrl <- Box(metadata.addURL) ?~ {"the view " + viewId + "does not allow adding a url"}
          urlJson <- tryo{(json.extract[UrlJSON])} ?~ {"wrong JSON format"}
          if(addUrl(urlJson.URL))
        } yield {
            val successJson = SuccessMessage("url added")
            successJsonResponse(Extraction.decompose(successJson), 201)
        }
    }
  })

  oauthServe(apiPrefix{
    case "banks" :: bankId :: "accounts" :: accountId :: viewId :: "other_accounts":: other_account_id :: "url" :: Nil JsonPut json -> _ => {
      user =>
        for {
          account <- BankAccount(bankId, accountId)
          view <- View.fromUrl(viewId)
          otherBankAccount <- account.moderatedOtherBankAccount(other_account_id, view, user)
          metadata <- Box(otherBankAccount.metadata) ?~ {"the view " + viewId + "does not allow metadata access"}
          addUrl <- Box(metadata.addURL) ?~ {"the view " + viewId + "does not allow updating a url"}
          urlJson <- tryo{(json.extract[UrlJSON])} ?~ {"wrong JSON format"}
          if(addUrl(urlJson.URL))
        } yield {
            val successJson = SuccessMessage("url updated")
            successJsonResponse(Extraction.decompose(successJson))
        }
    }
  })

  oauthServe(apiPrefix{
    case "banks" :: bankId :: "accounts" :: accountId :: viewId :: "other_accounts":: other_account_id :: "url" :: Nil JsonDelete _ => {
      user =>
        for {
          account <- BankAccount(bankId, accountId)
          view <- View.fromUrl(viewId)
          otherBankAccount <- account.moderatedOtherBankAccount(other_account_id, view, user)
          metadata <- Box(otherBankAccount.metadata) ?~ {"the view " + viewId + "does not allow metadata access"}
          addUrl <- Box(metadata.addURL) ?~ {"the view " + viewId + "does not allow deleting a url"}
          if(addUrl(""))
        } yield noContentJsonResponse
    }
  })

  oauthServe(apiPrefix{
    case "banks" :: bankId :: "accounts" :: accountId :: viewId :: "other_accounts":: other_account_id :: "image_url" :: Nil JsonPost json -> _ => {
      user =>
        for {
          account <- BankAccount(bankId, accountId)
          view <- View.fromUrl(viewId)
          otherBankAccount <- account.moderatedOtherBankAccount(other_account_id, view, user)
          metadata <- Box(otherBankAccount.metadata) ?~ {"the view " + viewId + "does not allow metadata access"}
          addImageUrl <- Box(metadata.addImageURL) ?~ {"the view " + viewId + "does not allow adding an image url"}
          imageUrlJson <- tryo{(json.extract[ImageUrlJSON])} ?~ {"wrong JSON format"}
          if(addImageUrl(imageUrlJson.image_URL))
        } yield {
            val successJson = SuccessMessage("image url added")
            successJsonResponse(Extraction.decompose(successJson), 201)
        }
    }
  })

  oauthServe(apiPrefix{
    case "banks" :: bankId :: "accounts" :: accountId :: viewId :: "other_accounts":: other_account_id :: "image_url" :: Nil JsonPut json -> _ => {
      user =>
        for {
          account <- BankAccount(bankId, accountId)
          view <- View.fromUrl(viewId)
          otherBankAccount <- account.moderatedOtherBankAccount(other_account_id, view, user)
          metadata <- Box(otherBankAccount.metadata) ?~ {"the view " + viewId + "does not allow metadata access"}
          addImageUrl <- Box(metadata.addImageURL) ?~ {"the view " + viewId + "does not allow updating an image url"}
          imageUrlJson <- tryo{(json.extract[ImageUrlJSON])} ?~ {"wrong JSON format"}
          if(addImageUrl(imageUrlJson.image_URL))
        } yield {
            val successJson = SuccessMessage("image url updated")
            successJsonResponse(Extraction.decompose(successJson))
        }
    }
  })

  oauthServe(apiPrefix{
    case "banks" :: bankId :: "accounts" :: accountId :: viewId :: "other_accounts":: other_account_id :: "image_url" :: Nil JsonDelete _ => {
      user =>
        for {
          account <- BankAccount(bankId, accountId)
          view <- View.fromUrl(viewId)
          otherBankAccount <- account.moderatedOtherBankAccount(other_account_id, view, user)
          metadata <- Box(otherBankAccount.metadata) ?~ {"the view " + viewId + "does not allow metadata access"}
          addImageUrl <- Box(metadata.addImageURL) ?~ {"the view " + viewId + "does not allow deleting an image url"}
          if(addImageUrl(""))
        } yield noContentJsonResponse
    }
  })

  oauthServe(apiPrefix{
    case "banks" :: bankId :: "accounts" :: accountId :: viewId :: "other_accounts":: other_account_id :: "open_corporates_url" :: Nil JsonPost json -> _ => {
      user =>
        for {
          account <- BankAccount(bankId, accountId)
          view <- View.fromUrl(viewId)
          otherBankAccount <- account.moderatedOtherBankAccount(other_account_id, view, user)
          metadata <- Box(otherBankAccount.metadata) ?~ {"the view " + viewId + "does not allow metadata access"}
          addOpenCorpUrl <- Box(metadata.addOpenCorporatesURL) ?~ {"the view " + viewId + "does not allow adding an open corporate url"}
          opernCoprUrl <- tryo{(json.extract[OpenCorporateUrlJSON])} ?~ {"wrong JSON format"}
          if(addOpenCorpUrl(opernCoprUrl.open_corporates_URL))
        } yield {
            val successJson = SuccessMessage("open corporate url added")
            successJsonResponse(Extraction.decompose(successJson), 201)
        }
    }
  })

  oauthServe(apiPrefix{
    case "banks" :: bankId :: "accounts" :: accountId :: viewId :: "other_accounts":: other_account_id :: "open_corporates_url" :: Nil JsonPut json -> _ => {
      user =>
        for {
          account <- BankAccount(bankId, accountId)
          view <- View.fromUrl(viewId)
          otherBankAccount <- account.moderatedOtherBankAccount(other_account_id, view, user)
          metadata <- Box(otherBankAccount.metadata) ?~ {"the view " + viewId + "does not allow metadata access"}
          addOpenCorpUrl <- Box(metadata.addOpenCorporatesURL) ?~ {"the view " + viewId + "does not allow updating an open corporate url"}
          opernCoprUrl <- tryo{(json.extract[OpenCorporateUrlJSON])} ?~ {"wrong JSON format"}
          if(addOpenCorpUrl(opernCoprUrl.open_corporates_URL))
        } yield {
            val successJson = SuccessMessage("open corporate url updated")
            successJsonResponse(Extraction.decompose(successJson))
        }
    }
  })

  oauthServe(apiPrefix{
    case "banks" :: bankId :: "accounts" :: accountId :: viewId :: "other_accounts":: other_account_id :: "open_corporates_url" :: Nil JsonDelete _ => {
      user =>
        for {
          account <- BankAccount(bankId, accountId)
          view <- View.fromUrl(viewId)
          otherBankAccount <- account.moderatedOtherBankAccount(other_account_id, view, user)
          metadata <- Box(otherBankAccount.metadata) ?~ {"the view " + viewId + "does not allow metadata access"}
          addOpenCorpUrl <- Box(metadata.addOpenCorporatesURL) ?~ {"the view " + viewId + "does not allow deleting an open corporate url"}
          if(addOpenCorpUrl(""))
        } yield noContentJsonResponse
    }
  })

  oauthServe(apiPrefix{
    case "banks" :: bankId :: "accounts" :: accountId :: viewId :: "other_accounts" :: other_account_id :: "corporate_location" :: Nil JsonPost json -> _ => {
      user =>
        for {
          u <- user
          account <- BankAccount(bankId, accountId)
          view <- View.fromUrl(viewId)
          otherBankAccount <- account.moderatedOtherBankAccount(other_account_id, view, user)
          metadata <- Box(otherBankAccount.metadata) ?~ {"the view " + viewId + "does not allow metadata access"}
          addCorpLocation <- Box(metadata.addCorporateLocation) ?~ {"the view " + viewId + "does not allow adding a corporate location"}
          openCorpLocation <- tryo{(json.extract[CorporateLocationJSON])} ?~ {"wrong JSON format"}
          if(addCorpLocation(u.id_, view.id, (now:TimeSpan), openCorpLocation.corporate_location.longitude, openCorpLocation.corporate_location.latitude))
        } yield {
            val successJson = SuccessMessage("corporate location added")
            successJsonResponse(Extraction.decompose(successJson), 201)
        }
    }
  })

  oauthServe(apiPrefix{
    case "banks" :: bankId :: "accounts" :: accountId :: viewId :: "other_accounts":: other_account_id :: "corporate_location" :: Nil JsonPut json -> _ => {
      user =>
        for {
          u <- user
          account <- BankAccount(bankId, accountId)
          view <- View.fromUrl(viewId)
          otherBankAccount <- account.moderatedOtherBankAccount(other_account_id, view, user)
          metadata <- Box(otherBankAccount.metadata) ?~ {"the view " + viewId + "does not allow metadata access"}
          addCorpLocation <- Box(metadata.addCorporateLocation) ?~ {"the view " + viewId + "does not allow updating a corporate location"}
          openCorpLocation <- tryo{(json.extract[CorporateLocationJSON])} ?~ {"wrong JSON format"}
         if(addCorpLocation(u.id_, view.id, now, openCorpLocation.corporate_location.longitude, openCorpLocation.corporate_location.latitude))
        } yield {
            val successJson = SuccessMessage("corporate location updated")
            successJsonResponse(Extraction.decompose(successJson))
        }
    }
  })

  oauthServe(apiPrefix{
    case "banks" :: bankId :: "accounts" :: accountId :: viewId :: "other_accounts":: other_account_id :: "corporate_location" :: Nil JsonDelete _ => {
      user =>
        for {
          u <- user
          account <- BankAccount(bankId, accountId)
          view <- View.fromUrl(viewId)
          otherBankAccount <- account.moderatedOtherBankAccount(other_account_id, view, user)
          metadata <- Box(otherBankAccount.metadata) ?~ {"the view " + viewId + "does not allow metadata access"}
          deleted <- Box(metadata.deleteCorporateLocation)
        } yield {
          deleted(view.id)
          noContentJsonResponse
        }
    }
  })

  oauthServe(apiPrefix{
    case "banks" :: bankId :: "accounts" :: accountId :: viewId :: "other_accounts" :: other_account_id :: "physical_location" :: Nil JsonPost json -> _ => {
      user =>
        for {
          u <- user
          account <- BankAccount(bankId, accountId)
          view <- View.fromUrl(viewId)
          otherBankAccount <- account.moderatedOtherBankAccount(other_account_id, view, user)
          metadata <- Box(otherBankAccount.metadata) ?~ {"the view " + viewId + "does not allow metadata access"}
          addPhysicalLocation <- Box(metadata.addPhysicalLocation) ?~ {"the view " + viewId + "does not allow adding a physical location"}
          openPhysicalLocation <- tryo{(json.extract[PhysicalLocationJSON])} ?~ {"wrong JSON format"}
          if(addPhysicalLocation(u.id_, view.id, now, openPhysicalLocation.physical_location.longitude, openPhysicalLocation.physical_location.latitude))
        } yield {
            val successJson = SuccessMessage("physical location added")
            successJsonResponse(Extraction.decompose(successJson), 201)
        }
    }
  })

  oauthServe(apiPrefix{
    case "banks" :: bankId :: "accounts" :: accountId :: viewId :: "other_accounts":: other_account_id :: "physical_location" :: Nil JsonPut json -> _ => {
      user =>
        for {
          u <- user
          account <- BankAccount(bankId, accountId)
          view <- View.fromUrl(viewId)
          otherBankAccount <- account.moderatedOtherBankAccount(other_account_id, view, user)
          metadata <- Box(otherBankAccount.metadata) ?~ {"the view " + viewId + "does not allow metadata access"}
          addPhysicalLocation <- Box(metadata.addPhysicalLocation) ?~ {"the view " + viewId + "does not allow updating a physical location"}
          openPhysicalLocation <- tryo{(json.extract[PhysicalLocationJSON])} ?~ {"wrong JSON format"}
         if(addPhysicalLocation(u.id_, view.id, now, openPhysicalLocation.physical_location.longitude, openPhysicalLocation.physical_location.latitude))
        } yield {
            val successJson = SuccessMessage("physical location updated")
            successJsonResponse(Extraction.decompose(successJson))
        }
    }
  })

  oauthServe(apiPrefix{
    case "banks" :: bankId :: "accounts" :: accountId :: viewId :: "other_accounts":: other_account_id :: "physical_location" :: Nil JsonDelete _ => {
      user =>
        for {
          u <- user
          account <- BankAccount(bankId, accountId)
          view <- View.fromUrl(viewId)
          otherBankAccount <- account.moderatedOtherBankAccount(other_account_id, view, user)
          metadata <- Box(otherBankAccount.metadata) ?~ {"the view " + viewId + "does not allow metadata access"}
          deleted <- Box(metadata.deletePhysicalLocation)
        } yield {
          deleted(view.id)
          noContentJsonResponse
        }
    }
  })
  
  oauthServe(apiPrefix {
    case "banks" :: bankId :: "accounts" :: accountId :: viewId :: "transactions" :: Nil JsonGet json => {
      user =>
        
      def asInt(s: Box[String], default: Int): Int = {
        s match {
          case Full(str) => tryo { str.toInt } getOrElse default
          case _ => default
        }
      }
      
      val limit = asInt(json.header("obp_limit"), 50)
      val offset = asInt(json.header("obp_offset"), 0)
      
       /**
       * sortBy is currently disabled as it would open up a security hole:
       *
       * sortBy as currently implemented will take in a parameter that searches on the mongo field names. The issue here
       * is that it will sort on the true value, and not the moderated output. So if a view is supposed to return an alias name
       * rather than the true value, but someone uses sortBy on the other bank account name/holder, not only will the returned data
       * have the wrong order, but information about the true account holder name will be exposed due to its position in the sorted order
       *
       * This applies to all fields that can have their data concealed... which in theory will eventually be most/all
       *
       */
      //val sortBy = json.header("obp_sort_by")
      val sortBy = None
      val sortDirection = OBPOrder(json.header("obp_sort_by"))
      val fromDate = tryo{dateFormat.parse(json.header("obp_from_date") getOrElse "")}.map(OBPFromDate(_))
      val toDate = tryo{dateFormat.parse(json.header("obp_to_date") getOrElse "")}.map(OBPToDate(_))

      def getTransactions(bankAccount: BankAccount, view: View, user: Option[User]) = {
        if(bankAccount.authorizedAccess(view, user)) {
          val basicParams = List(OBPLimit(limit),
                          OBPOffset(offset),
                          OBPOrdering(sortBy, sortDirection))

          val params : List[OBPQueryParam] = fromDate.toList ::: toDate.toList ::: basicParams
          bankAccount.getModeratedTransactions(params: _*)(view.moderate)
        } else Nil
      }
      
       for {
        bankAccount <- BankAccount(bankId, accountId)
        view <- View.fromUrl(viewId) //TODO: This will have to change if we implement custom view names for different accounts
      } yield {
        val ts = getTransactions(bankAccount, view, user)
        val json = JSONFactory.createTransactionsJSON(ts)
        successJsonResponse(Extraction.decompose(json))
      }
      
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
    case "banks" :: bankId :: "accounts" :: accountId :: viewId :: "transactions" :: transactionId :: "metadata" :: "comments" :: Nil JsonPost json -> _ => {
      user =>
        for {
          commentJson <- tryo{json.extract[PostTransactionCommentJSON]}
          u <- user
          view <- View.fromUrl(viewId)
          metadata <- moderatedTransactionMetadata(bankId, accountId, view.permalink, transactionId, Full(u))
          addCommentFunc <- Box(metadata.addComment) ?~ {"view " + viewId + " does not authorize adding comments"}
          postedComment <- Full(addCommentFunc(u.id_, view.id, commentJson.value, now))
        } yield {
          successJsonResponse(Extraction.decompose(JSONFactory.createTransactionCommentJSON(postedComment)))
        }
    }
  })
  oauthServe(apiPrefix {
    //post a tag
    case "banks" :: bankId :: "accounts" :: accountId :: viewId :: "transactions" :: transactionID :: "metadata" :: "tags" :: Nil JsonPost json -> _ => {

      user =>
        for {
          tagJson <- tryo{json.extract[PostTransactionTagJSON]}
          u <- user
          view <- View.fromUrl(viewId)
          metadata <- moderatedTransactionMetadata(bankId, accountId, view.permalink, transactionID, Full(u))
          addTagFunc <- Box(metadata.addTag) ?~ {"view " + viewId + " does not authorize adding tags"}
          postedTag <- Full(addTagFunc(u.id_, view.id, tagJson.value, now))
        } yield {
          successJsonResponse(Extraction.decompose(JSONFactory.createTransactionTagJSON(postedTag)))
        }
    }
  })

  oauthServe(apiPrefix {
    //delete a tag
    case "banks" :: bankId :: "accounts" :: accountId :: viewId :: "transactions" :: transactionId :: "metadata" :: "tags" :: tagId :: Nil JsonDelete _ => {

      user =>
        for {
          view <- View.fromUrl(viewId)
          metadata <- moderatedTransactionMetadata(bankId, accountId, viewId, transactionId, user)
          bankAccount <- BankAccount(bankId, accountId)
          deleted <- Box(metadata.deleteTag(tagId, user, bankAccount))
        } yield {
          successJsonResponse(204)
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
          addImageFunc <- Box(metadata.addImage) ?~ {"view " + viewId + " does not authorize adding images"}
          url <- tryo{new URL(imageJson.URL)} ?~! "Could not parse url string as a valid URL"
          postedImage <- Full(addImageFunc(u.id_, view.id, imageJson.label, now, url))
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
          view <- View.fromUrl(viewId)
          bankAccount <- BankAccount(bankId, accountId)
          deleted <- Box(metadata.deleteImage(imageId, user, bankAccount))
        } yield {
          noContentJsonResponse
        }
    }
  })
}