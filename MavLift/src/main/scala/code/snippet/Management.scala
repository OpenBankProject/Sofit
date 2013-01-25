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
import code.model.dataAccess.{Account,OtherAccount}
import net.liftweb.util.Helpers._
import scala.xml.NodeSeq
import scala.xml.Text
import net.liftweb.http.{S, SHtml}
import net.liftweb.http.js.JsCmds.Noop
import net.liftweb.common.Full
import net.liftweb.common.Empty
import code.widgets.tableSorter.{CustomTableSorter, DisableSorting, Sorting, Sorter}
import net.liftweb.http.js.JsCmd

class Management(currentAccount : Account) {

  val headers = (0, Sorter("text")) :: (5, DisableSorting()) :: (6, DisableSorting()) :: Nil
  val sortList = (0, Sorting.ASC) :: Nil
  
  val options = CustomTableSorter.options(headers, sortList)
  
  def tableSorter(xhtml: NodeSeq) : NodeSeq = {
    CustomTableSorter("#other_acc_management", options)
  }
  
  def showAll(xhtml: NodeSeq) : NodeSeq = {
    
    def getMostUpToDateOtherAccount(holder: String) = {
      currentAccount.otherAccounts.objs.find(o => {
        o.holder.get.equals(holder)
      })
    }
    
    def editablePublicAlias(initialValue : String, holder: String) = {
      def alterPublicAlias = (oAccount: OtherAccount, newValue: String) => oAccount.publicAlias(newValue)
      editable(initialValue, holder, alterPublicAlias, "Public Alias")
    }
    
    def editablePrivateAlias(initialValue : String, holder: String) = {
      def alterPrivateAlias = (oAccount: OtherAccount, newValue: String) => oAccount.privateAlias(newValue)
      editable(initialValue, holder, alterPrivateAlias, "Private Alias")
    }
    
    def editableImageUrl(initialValue : String, holder: String) = {
      def alterImageUrl = (oAccount: OtherAccount, newValue: String) => oAccount.imageUrl(newValue)
      editable(initialValue, holder, alterImageUrl, "Image URL")
    }
    
    def editableUrl(initialValue : String, holder: String) = {
      def alterUrl = (oAccount: OtherAccount, newValue: String) => oAccount.url(newValue)
      editable(initialValue, holder, alterUrl, "Website")
    }
    
    def editableMoreInfo(initialValue : String, holder: String) = {
      def moreInfo = (oAccount: OtherAccount, newValue: String) => oAccount.moreInfo(newValue)
      editable(initialValue, holder, moreInfo, "Information")
    }
    
    def editableOpenCorporatesUrl(initialValue : String, holder: String) = {
      def openCorporatesUrl = (oAccount: OtherAccount, newValue: String) => oAccount.openCorporatesUrl(newValue)
      editable(initialValue, holder, openCorporatesUrl, "Open Corporates URL")
    }
    
    def editable(
      initialValue: String, 
      holder: String,  
      alterOtherAccount: (OtherAccount, String) => OtherAccount,
      defaultValue: String ) = {
      var currentValue = initialValue
      
      def saveValue() = {
        val otherAcc = getMostUpToDateOtherAccount(holder)
        if(otherAcc.isDefined)
          alterOtherAccount(otherAcc.get, currentValue).save
      }
      
      CustomEditable.editable(currentValue, SHtml.text(currentValue, currentValue = _), () =>{
        saveValue()
        Noop
      }, defaultValue)
    }
    
    currentAccount.otherAccounts.objs.sortBy(_.holder.get).flatMap(other => {
      
      val account = other.holder.get
      val publicAlias = other.publicAlias.get
      val privateAlias = other.privateAlias.get
      val moreInfo = other.moreInfo.get
      val website = other.url.get
      val openCorporates = other.openCorporatesUrl.get
      val imageURL = other.imageUrl.get
      
      val accountSelector = ".account *" #> account
      
      val publicSelector = ".public *" #> editablePublicAlias(publicAlias, account)
      
      val privateSelector = ".private *" #> editablePrivateAlias(privateAlias, account)
      
      val websiteSelector = ".website *" #> editableUrl(website, account)
      
      val openCorporatesSelector = ".open_corporates *" #> editableOpenCorporatesUrl(openCorporates, account)
      
      val moreInfoSelector = ".information *" #> editableMoreInfo(moreInfo, account)
      
      val imageURLSelector = ".imageURL *" #> editableImageUrl(imageURL, account)
      
      (accountSelector &
       publicSelector &
       privateSelector &
       websiteSelector &
       openCorporatesSelector &
       moreInfoSelector &
       imageURLSelector).apply(xhtml)
    })
  }
  
}