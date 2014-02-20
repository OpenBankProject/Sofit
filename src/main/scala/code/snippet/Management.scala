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
import net.liftweb.json.JsonAST._
import net.liftweb.util.Helpers._
import scala.xml.NodeSeq
import scala.xml.Text
import net.liftweb.http.{S, SHtml}
import net.liftweb.http.js.JsCmds.Noop
import net.liftweb.common.Full
import net.liftweb.common.Empty
import code.widgets.tableSorter.{CustomTableSorter, DisableSorting, Sorting, Sorter}
import net.liftweb.http.js.JsCmd
import code.lib.ObpJson.OtherAccountsJson
import code.lib.ObpJson.OtherAccountJson
import code.lib.ObpDelete
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

  def tableSorter(xhtml: NodeSeq) : NodeSeq = {
    CustomTableSorter("#other_acc_management", options)
  }

  def showAll(xhtml: NodeSeq) : NodeSeq = {

    def editablePublicAlias(initialValue : String, holder: String, otherAccountId: String) = {
      apiEditable(initialValue, holder, "alias", "public_alias", otherAccountId, "Public Alias")
    }

    def editablePrivateAlias(initialValue : String, holder: String, otherAccountId: String) = {
      apiEditable(initialValue, holder, "alias", "private_alias", otherAccountId, "Private Alias")
    }

    def editableImageUrl(initialValue : String, holder: String, otherAccountId: String) = {
      apiEditable(initialValue, holder, "image_URL", "image_url", otherAccountId, "Image URL")
    }

    def editableUrl(initialValue : String, holder: String, otherAccountId: String) = {
      apiEditable(initialValue, holder, "URL", "url", otherAccountId, "Website")
    }

    def editableMoreInfo(initialValue : String, holder: String, otherAccountId: String) = {
      apiEditable(initialValue, holder, "more_info", "more_info", otherAccountId, "Information")
    }

    def editableOpenCorporatesUrl(initialValue : String, holder: String, otherAccountId: String) = {
      apiEditable(initialValue, holder, "open_corporates_URL", "open_corporates_url", otherAccountId, "Open Corporates URL")
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
      defaultValue: String ) = {
      var currentValue = initialValue
      var exists = !currentValue.isEmpty

      def json() : JValue = (jsonKey -> currentValue)
        
      def saveValue() = {
        if(currentValue.isEmpty) {
          //Send a delete
          ObpDelete("/v1.2/banks/" + urlParams.bankId + "/accounts/" + urlParams.accountId + "/owner/other_accounts/" + otherAccountId + "/" + apiProperty)
          exists = false
        } else {
          if(exists) {
            ObpPut("/v1.2/banks/" + urlParams.bankId + "/accounts/" + urlParams.accountId + "/owner/other_accounts/" + otherAccountId + "/" + apiProperty,
                json())
          } else {
            ObpPost("/v1.2/banks/" + urlParams.bankId + "/accounts/" + urlParams.accountId + "/owner/other_accounts/" + otherAccountId + "/" + apiProperty,
                json())
            exists = true
          }
        }
      }
      
      CustomEditable.editable(currentValue, SHtml.text(currentValue, currentValue = _), () =>{
        saveValue()
        Noop
      }, defaultValue)
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