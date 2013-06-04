/**
 * Open Bank Project - Transparency / Social Finance Web Application
 * Copyright (C) 2011, 2012, TESOBE / Music Pictures Ltd
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * Email: contact@tesobe.com
 * TESOBE / Music Pictures Ltd
 * Osloerstrasse 16/17
 * Berlin 13359, Germany
 *
 * This product includes software developed at
 * TESOBE (http://www.tesobe.com/)
 * by
 * Simon Redfern : simon AT tesobe DOT com
 * Stefan Bethge : stefan AT tesobe DOT com
 * Everett Sochowski : everett AT tesobe DOT com
 * Ayoub Benali: ayoub AT tesobe DOT com
 *
 */

package code.snippet
import scala.xml.NodeSeq
import net.liftweb.http.S
import net.liftweb.http.LiftRules
import net.liftweb.util.Helpers
import net.liftweb.util.Helpers._
import scala.xml.Group
import net.liftweb.sitemap.Loc
import net.liftweb.common.Box
import net.liftweb.common.Full
import net.liftweb.common.Empty
import net.liftweb.sitemap.SiteMapSingleton
import code.model.dataAccess.{ OBPUser, Account, LocalStorage }
import net.liftweb.http.SHtml
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds._Noop
import code.model.BankAccount
import code.lib.ObpJson._
import code.lib.ObpGet
import code.lib.ObpAPI

class Nav {

  val url = S.uri.split("/", 0)
  val accountJson : Option[AccountJson]= {
    if (url.size > 5) {
      val bankId = url(2)
      val accountId = url(4)
      val viewId = url(5)
      ObpAPI.account(bankId, accountId, viewId)
    } else {
      None
    }
  }
  val viewJsons: List[ViewJson] = {
    accountJson.flatMap(accJson => {
      accJson.views_available
    }).flatten.toList
  }

  def eraseMenu =
    "* * " #> ""
  def views: net.liftweb.util.CssSel = {
    val url = S.uri.split("/", 0)
    if (url.size > 4) {
      ".navitem *" #> {
        viewJsons.map(viewJson => {
          val viewUrl = "/banks/" + url(2) + "/accounts/" + url(4) + "/" + viewJson.id.getOrElse("")
          ".navlink [href]" #> viewUrl &
          ".navlink *" #> viewJson.short_name.getOrElse("") &
          ".navlink [class+]" #> markIfSelected(viewUrl)
        })
      }
    } else
      eraseMenu
  }

  def management = {
    val url = S.uri.split("/", 0)

    def getManagement = for {
      user <- OBPUser.currentUser
      bankAccount <- BankAccount(url(2), url(4))
      if (user.hasMangementAccess(bankAccount))
    } yield {
      val managementUrl = "/banks/" + url(2) + "/accounts/" + url(4) + "/management"
      ".navlink [href]" #> { managementUrl } &
        ".navlink *" #> "Management" &
        ".navlink [class+]" #> markIfSelected(managementUrl)
    }

    if (url.size > 4) getManagement getOrElse eraseMenu
    else eraseMenu
  }

  def item = {
    val name = S.attr("name").getOrElse("")
    val loc = (for {
      sitemap <- LiftRules.siteMap
      l <- new SiteMapSingleton().findAndTestLoc(name)
    } yield l)

    ".navitem *" #> {
      loc.map(navItemSelector)
    }
  }

  def navItemSelector(l: Loc[_]) = {
    ".navlink [href]" #> l.calcDefaultHref &
      ".navlink *" #> l.linkText &
      ".navlink [class+]" #> markIfSelected(l.calcDefaultHref)
  }

  def onlyOnSomePages = {
    val pages: List[String] = S.attr("pages").map(_.toString.split(",").toList).getOrElse(Nil)

    val locs = pages.flatMap(page => (for {
      sitemap <- LiftRules.siteMap
      l <- new SiteMapSingleton().findAndTestLoc(page)
    } yield l))

    val isPage = locs.map(l => {
      //hack due to deadline to fix / and /index being the same
      val currentPage = if (S.uri == "/") "/index" else S.uri
      (l.calcDefaultHref == currentPage)
    }).exists(_ == true)

    if (isPage) item
    else "* *" #> ""
  }

  def privilegeAdmin = {
    val url = S.uri.split("/", 0)

    def hide = ".navitem *" #> ""
    def getPrivilegeAdmin = for {
      bankAccount <- BankAccount(url(2), url(4))
      if (OBPUser.hasOwnerPermission(bankAccount))
      loc <- new SiteMapSingleton().findAndTestLoc("Privilege Admin")
    } yield {
      ".navitem *" #> {
        ".navlink [href]" #> loc.calcDefaultHref &
          ".navlink *" #> loc.linkText &
          ".navlink [class+]" #> markIfSelected(loc.calcDefaultHref)
      }
    }

    if (url.size > 4) getPrivilegeAdmin.getOrElse(hide) else hide
  }

  def markIfSelected(href: String): Box[String] = {
    val currentHref = S.uri
    if (href.equals(currentHref)) Full("selected")
    else Empty
  }

  def listAccounts = {
    //TODO: Get all banks, then for each bank, all accounts
    val banksJsonBox = ObpAPI.allBanks
    val bankJsons : List[BankJson] = banksJsonBox.map(_.bankJsons).toList.flatten
    val accountJsons : List[AccountJson] = Nil //TODO: Need to know a view to make this request...
    
    type AccountId = String
    type AccountLabel = String
    val availableAccounts: List[(AccountId, AccountLabel)] = {
      accountJson.toList.map(accountJson => {
        val id = accountJson.id.getOrElse("")
        val label = "TODO" //TODO: How can I get the account label?
        (id, label)
      })
    }

    def computeDefaultValue: Box[String] =
      {
        val url = S.uri.split("/", 0)
        var output = "0"
        if (url.size > 4)
          output = url(2) + "," + url(4)
        Full(output)
      }

    def redirect(selectValue: String): JsCmd =
      {
        val bankAndAccount = selectValue.split(",", 0)
        if (bankAndAccount.size != 2) {
          _Noop
        } else {
          val bankId = bankAndAccount(0)
          val accountId = bankAndAccount(1)
          //TODO : the account may not has an public view, so this redirection would retun a 404
          //a better solution has to be found
          S.redirectTo("/banks/" + bankId + "/accounts/" + bankId + "/public")
        }
      }
    
    "#accountList *" #> {
      computeDefaultValue match {
        case Full("postbank,tesobe") =>
          //For now we're only showing this list of available accounts on the TESOBE account page
          SHtml.ajaxSelect(availableAccounts, computeDefaultValue, redirect _)
        case _ =>
          NodeSeq.Empty
      }
    } //TODO: Test the above and remove the below if it works
    
    var accounts: List[(String, String)] = List()
    OBPUser.currentUser match {
      case Full(user) => Account.findAll.map(account => {
        val bankAccount = Account.toBankAccount(account)
        if (user.permittedViews(bankAccount).size != 0)
          accounts ::= (account.bankPermalink + "," + account.permalink, account.bankName + " - " + account.name)
      })
      case _ => Account.findAll.map(account =>
        if (account.anonAccess.is)
          accounts ::= (account.bankPermalink + "," + account.permalink, account.bankName + " - " + account.name))
    }
    accounts ::= ("0", "--> Choose an account")
    def redirect2(selectValue: String): JsCmd =
      {
        val bankAndaccount = selectValue.split(",", 0)
        if (bankAndaccount.size == 2)
          if (LocalStorage.correctBankAndAccount(bankAndaccount(0), bankAndaccount(1)))
            //TODO : the account may not has an public view, so this redirection would retun a 404
            //a better solution has to be found
            S.redirectTo("/banks/" + bankAndaccount(0) + "/accounts/" + bankAndaccount(1) + "/public")
          else
            _Noop
        else
          _Noop
      }
    
    "#accountList *" #> {
      computeDefaultValue match {
        case Full("postbank,tesobe") =>
          SHtml.ajaxSelect(accounts, computeDefaultValue, redirect2 _)
        case _ =>
          NodeSeq.Empty
      }
    }
  }
}