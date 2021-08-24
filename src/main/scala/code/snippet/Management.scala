/**
Open Bank Project - Sofi Web Application
Copyright (C) 2011 - 2021, TESOBE GmbH

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
TESOBE GmbH.
Osloer Str. 16/17
Berlin 13359, Germany

  This product includes software developed at
  TESOBE (http://www.tesobe.com/)
  by
  Simon Redfern : simon AT tesobe DOT com
  Stefan Bethge : stefan AT tesobe DOT com
  Everett Sochowski : everett AT tesobe DOT com
  Ayoub Benali: ayoub AT tesobe DOT com
  Nina Gänsdorfer: nina AT tesobe.com

 */

package code.snippet

import code.Constant.versionOfApi
import code.util.Helper._
import net.liftweb.json.JsonAST._
import net.liftweb.util.Helpers._
import scala.xml.NodeSeq
import net.liftweb.http.SHtml
import net.liftweb.http.js.JsCmds.{Confirm, Alert, Noop}
import code.widgets.tableSorter.{CustomTableSorter, DisableSorting, Sorting, Sorter}
import code.lib.ObpJson.OtherAccountsJson
import code.lib.ObpJson.OtherAccountJson
import code.lib.ObpDeleteBoolean
import code.lib.ObpPost
import code.lib.ObpPut
import net.liftweb.json.JsonDSL._

case class ManagementURLParams(bankId: String, accountId: String)

class Management(params : (OtherAccountsJson, ManagementURLParams)) {

  val otherAccountsJson = params._1
  val urlParams = params._2
  
  val headers = (0, Sorter("text")) :: (5, DisableSorting()) :: (6, DisableSorting()) :: Nil
  val sortList = (0, Sorting.ASC) :: Nil

  val options = CustomTableSorter.options(headers, sortList)


  def accountTitle = ".account_title *" #> getAccountTitle(urlParams.bankId, urlParams.accountId)

  def tableSorter(xhtml: NodeSeq) : NodeSeq = {
    CustomTableSorter("#other_acc_management", options)
  }

  def showAll(xhtml: NodeSeq) : NodeSeq = {

    def editablePublicAlias(initialValue : String, holder: String, otherAccountId: String) = {
      apiAliasEditable(initialValue, holder, "alias", "public_alias", otherAccountId, displayHolder(holder, "Public Alias"), true)
    }

    def editablePrivateAlias(initialValue : String, holder: String, otherAccountId: String) = {
      apiAliasEditable(initialValue, holder, "alias", "private_alias", otherAccountId, displayHolder(holder, "Private Alias"), true)
    }

    def editableImageUrl(initialValue : String, holder: String, otherAccountId: String) = {
      apiEditable(initialValue, holder, "image_URL", "image_url", otherAccountId, "Image URL", false)
    }

    def editableUrl(initialValue : String, holder: String, otherAccountId: String) = {
      apiEditable(initialValue, holder, "URL", "url", otherAccountId, "Website", false)
    }

    def editableMoreInfo(initialValue : String, holder: String, otherAccountId: String) = {
      apiEditable(initialValue, holder, "more_info", "more_info", otherAccountId, "Information", false)
    }

    def editableOpenCorporatesUrl(initialValue : String, holder: String, otherAccountId: String) = {
      apiEditable(initialValue, holder, "open_corporates_URL", "open_corporates_url", otherAccountId, "Open Corporates URL", false)
    }
 
    def displayHolder(holder: String, default: String): String = {
      if (holder.isEmpty)
        default
      else
        "("+holder+")"
    }

    /**
     * TODO: If there are a lot of CustomEditables on a page, and lots of these pages open, that creates
     * a lot of closures to exist on the server. It would be good to refactor this to only require one function
     * per page on the server.
     */
    def apiEditable(
      initialValue: String,
      holder: String,
      jsonKey: String,
      apiProperty: String,
      otherAccountId: String,
      defaultValue: String,
      removable: Boolean ) = {
      var currentValue = initialValue
      var exists = currentValue.nonEmpty

      def json() : JValue = (jsonKey -> currentValue)

      def saveValue() = {
        if(currentValue.isEmpty) {
          //Send a delete
          ObpDeleteBoolean(s"/$versionOfApi/banks/" + urlParams.bankId + "/accounts/" + urlParams.accountId + "/owner/other_accounts/" + otherAccountId + "/metadata/" + apiProperty)
          exists = false
        } else {
          if(exists) {
            ObpPut(s"/$versionOfApi/banks/" + urlParams.bankId + "/accounts/" + urlParams.accountId + "/owner/other_accounts/" + otherAccountId + "/metadata/" + apiProperty,
              json())
          } else {
            ObpPost(s"/$versionOfApi/banks/" + urlParams.bankId + "/accounts/" + urlParams.accountId + "/owner/other_accounts/" + otherAccountId + "/metadata/" + apiProperty,
              json())
            exists = true
          }
        }
      }

      CustomEditable.editable(currentValue, SHtml.text(currentValue, currentValue = _, "class" -> "counterparties-input"),
        onSubmit = () => {
          saveValue()
          Noop
        },
        onDelete = () => {
          currentValue = ""
          saveValue()
        },
        defaultValue = defaultValue,
        removable = removable
      )
    }

    def apiAliasEditable(
      initialValue: String,
      holder: String,
      jsonKey: String,
      apiProperty: String,
      otherAccountId: String,
      defaultLabel: String, // is displayed when no alias is set
      removable: Boolean ) = {

      var currentValue = initialValue
      var inputDefaultValue = initialValue

      CustomEditable.editable(inputDefaultValue, SHtml.text(inputDefaultValue, currentValue = _, "class" -> "counterparties-input"),
        onSubmit = () => {
          if(currentValue.isEmpty || (holder.toLowerCase == currentValue.toLowerCase && apiProperty == "public_alias")){
            // delete Alias
            ObpDeleteBoolean(s"/$versionOfApi/banks/" + urlParams.bankId + "/accounts/" + urlParams.accountId + "/owner/other_accounts/" + otherAccountId + "/" + apiProperty)
            inputDefaultValue = ""
            Noop
          } else{
            val json = jsonKey -> currentValue
            ObpPut(s"/$versionOfApi/banks/" + urlParams.bankId + "/accounts/" + urlParams.accountId + "/owner/other_accounts/" + otherAccountId + "/" + apiProperty,
              json)
            inputDefaultValue = currentValue
            Noop
          }
        },
        onDelete = () => {
          ObpDeleteBoolean(s"/$versionOfApi/banks/" + urlParams.bankId + "/accounts/" + urlParams.accountId + "/owner/other_accounts/" + otherAccountId + "/" + apiProperty)
        },
        defaultValue = defaultLabel,
        removable = removable
      )
    }
    
    val otherAccountJsons : List[OtherAccountJson] = otherAccountsJson.other_accounts.getOrElse(Nil).
      sortBy[String](oAcc => oAcc.holder.flatMap(_.name).getOrElse(""))

    otherAccountJsons.flatMap {
      oAccJson => {
        
        val account = oAccJson.holder.flatMap(_.name).getOrElse("")
        val publicAlias = oAccJson.metadata.flatMap(_.public_alias).getOrElse("")
        val privateAlias = oAccJson.metadata.flatMap(_.private_alias).getOrElse("")
        val moreInfo = oAccJson.metadata.flatMap(_.more_info).getOrElse("")
        val website = oAccJson.metadata.flatMap(_.URL).getOrElse("")
        val openCorporates = oAccJson.metadata.flatMap(_.open_corporates_URL).getOrElse("")
        val imageURL = oAccJson.metadata.flatMap(_.image_URL).getOrElse("")
        val accountId = oAccJson.id.getOrElse("")
        
        
        val accountSelector = ".account *" #> account

        val accountIdSelector = ".account [id]" #> accountId

        val publicSelector = ".public *" #> editablePublicAlias(publicAlias, account, accountId)

        val privateSelector = ".private *" #> editablePrivateAlias(privateAlias, account, accountId)

        val websiteSelector = ".website *" #> editableUrl(website, account, accountId)

        val openCorporatesSelector = ".open_corporates *" #> editableOpenCorporatesUrl(openCorporates, account, accountId)

        val moreInfoSelector = ".information *" #> editableMoreInfo(moreInfo, account, accountId)

        val imageURLSelector = ".imageURL *" #> editableImageUrl(imageURL, account, accountId)

        (accountSelector &
          accountIdSelector &
          publicSelector &
          privateSelector &
          websiteSelector &
          openCorporatesSelector &
          moreInfoSelector &
          imageURLSelector).apply(xhtml)
      }
    }
  }
}
