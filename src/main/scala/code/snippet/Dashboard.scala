/**
Open Bank Project - Transparency / Social Finance Web Application
Copyright (C) 2011 - 2015, TESOBE  Ltd.

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
TESOBE Ltd.
Osloer Str. 16/17
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

import code.lib.OAuthClient
import code.lib.ObpAPI
import code.lib.ObpAPI._
import code.lib.ObpDelete
import code.lib.ObpGet
import code.lib.ObpJson.ResourceDoc
import code.lib.ObpPost
import code.lib.ObpPut
import net.liftweb.common.{Box, Failure, Empty, Full}
import code.lib.ObpJson._
import code.lib._
import net.liftweb.json.Serialization._

class OBPDashboardLeftSnippet (params : (TransactionsJson, AccountJson, TransactionsListURLParams, TransactionsJson, AccountJson, TransactionsListURLParams)) extends OBPTransactionSnippet ((params._1, params._2, params._3)) {
}

class OBPDashboardRightSnippet (params : (TransactionsJson, AccountJson, TransactionsListURLParams, TransactionsJson, AccountJson, TransactionsListURLParams)) extends OBPTransactionSnippet ((params._4, params._5, params._6)) {
}



///////////////



import _root_.net.liftweb._
import code.lib.ObpJson.{BarebonesAccountJson, BarebonesAccountsJson, ResourceDoc}
import code.lib._
import net.liftweb.http.js.jquery.JqJsCmds.DisplayMessage

//import code.snippet.CallUrlForm._
import net.liftweb.http.{SHtml, S}

import net.liftweb.json.{Extraction, JsonParser, JsonAST}
import net.liftweb.json.JsonAST.{JField, JObject, JValue}
import scala.xml.{XML, NodeSeq, Text}


import net.liftweb._
// for compact render
import net.liftweb.json._


import common._

import net.liftweb.util.Helpers._
import net.liftweb.http.SHtml.{text,ajaxSubmit, textarea, select, ajaxSelect}
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds.{Run, SetHtml}

import net.liftweb.json.Serialization.writePretty

import code.lib.ObpAPI.{getResourceDocsJson, allBanks, allAccountsAtOneBank}

import net.liftweb.http.CurrentReq


/*
Present a list of OBP resource URLs
 */
class OBPDashboard extends Loggable {



  val presetBankId = S.param("bank_id").getOrElse("")
  logger.info(s"bank_id in url param is $presetBankId")

  val presetAccountId = S.param("account_id").getOrElse("")
  logger.info(s"account_id in url param is $presetAccountId")

  val presetViewId = S.param("view_id").getOrElse("")
  logger.info(s"account_id in url param is $presetViewId")

  val presetCounterpartyId = S.param("counterparty_id").getOrElse("")
  logger.info(s"counterparty_id in url param is $presetCounterpartyId")

  val presetTransactionId = S.param("transaction_id").getOrElse("")
  logger.info(s"transaction_id in url param is $presetTransactionId")


  def stringToNodeSeq(html : String) : NodeSeq = {
    scala.xml.XML.loadString("<div>" + html + "</div>")
  }


  def modifiedRequestUrl(url: String, presetBankId: String, presetAccountId: String) = {
    // Potentially replace BANK_ID
    val url2: String = presetBankId match {
      case "" => url
      case _ => url.replaceAll("BANK_ID", presetBankId)
    }

    // Potentially replace ACCOUNT_ID
    val url3: String = presetAccountId match {
      case "" => url2
      case _ => url2.replaceAll("/ACCOUNT_ID", s"/$presetAccountId") // so we don't change OTHER_ACCOUNT_ID
    }

    // Potentially replace VIEW_ID
    val url4: String = presetViewId match {
      case "" => url3
      case _ => url3.replaceAll("VIEW_ID", presetViewId)
    }

    // Potentially replace OTHER_ACCOUNT_ID
    val url5: String = presetCounterpartyId match {
      case "" => url4
      case _ => url4.replaceAll("OTHER_ACCOUNT_ID", presetCounterpartyId)
    }

    // Potentially replace TRANSACTION_ID
    val url6: String = presetTransactionId match {
      case "" => url5
      case _ => url5.replaceAll("TRANSACTION_ID", presetTransactionId)
    }

    url6
  }

  def showAccountSelector = {


    val uri = CurrentReq.value.uri

    val banks = allBanks

    // TODO dehardcode the redirect path.


    def onAccountChange (v: Any) = {
      logger.info("account changed to " + v.toString)
      S.redirectTo(s"$uri?bank_id=${presetBankId}&account_id=${v}")
    }



    // Get a list of tuples List(("bank short name", "id"),("bank two", "id2")) to populate the drop down select list.
    // Could we write this in a way such that if there are no banks the doBankSelect is not run?
    val bankOptions = ("", "Select Bank") :: banks.map(b => b.bankJsons.map(bj => (bj.id.getOrElse(""), bj.short_name.getOrElse("") + " (" + bj.id.getOrElse("") + ")"))).getOrElse(List(("", "No Banks")))

    // TODO create BankId case class like in the API
    type BankID = String

    val privateAccountJsons : List[(String, String)] = for {
      privateAccountsJson <- ObpAPI.allAccountsAtOneBank(presetBankId).toList
      barebonesAccountJson <- privateAccountsJson.accounts.toList.flatten
      //bankId <- barebonesAccountJson.bank_id
      accountId <- barebonesAccountJson.id
      label <- barebonesAccountJson.label
    } yield (accountId, label)

    def getAccountOptions : List[(String,String)] = {

      val selectAccount = ("", "Select Account")
      val noneFound = ("", "") // No Accounts Found

      val options: List[(String, String)] = presetBankId match {
        case "" => List(noneFound)
        case _ => for {
          allAccountsJson <- ObpAPI.allAccountsAtOneBank(presetBankId).toList
          barebonesAccountJson <- allAccountsJson.accounts.toList.flatten
          accountId <- barebonesAccountJson.id
          label <- barebonesAccountJson.label
        } yield (accountId, label)
      }

      selectAccount :: options
    }

//    def getViewOptions : List[(String,String)] = {
//
//      val selectOne = OAuthClient.loggedIn match {
//        case true => ("", "Select View")
//        case false => ("", "Login for Views")
//      }
//
//      val noneFound = ("", "") // No Views Found
//
//      // TODO Should check for both presetBankId and presetAccountId
//      // Logged in user required?
//      val options: List[(String, String)] = presetAccountId match {
//        case "" => List(noneFound)
//        case _ => for {
//          views <- ObpAPI.getViewsForBankAccount(presetBankId, presetAccountId).toList
//          view <- views.views.toList.flatten
//          viewId <- view.id
//          shortName <- view.short_name
//        } yield (viewId, shortName)
//      }
//
//      selectOne :: options
//    }



    // Drop down box to select account. Selected item taken from url param.
    def doAccountSelect(in: NodeSeq) = ajaxSelect(getAccountOptions,
      Full(presetAccountId),
      v => onAccountChange(v))

    // Drop down box to select view for bank/account. Selected item taken from url param.
//    def doViewSelect(in: NodeSeq) = ajaxSelect(getViewOptions,
//      Full(presetViewId),
//      v => onViewChange(v))




    def loggedInStatusMessage = {
      if (OAuthClient.loggedIn) "" else "Some options and calls require login."
    }


      ".account_selector" #> doAccountSelect _
  }

}


