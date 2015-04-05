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
import net.liftweb.http.SHtml
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds._Noop
import code.lib.ObpJson._
import code.lib.ObpGet
import code.lib.ObpAPI
import code.lib.OAuthClient
import net.liftweb.util.CssSel



/*
TODO Fix the current behaviour where the current tab gets hidden and replaced with "Home"!
 */

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
    }).toList.flatten
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

    // Menu for a page which lists counterparties and their metadata (and edits the metadata)
    def getManagement = {
      val views = accountJson.flatMap(_.views_available).toList.flatten
      //TODO: Determine this in a better way
      val hasOwnerPermissions = views.exists(v => v.id == Some("owner"))
      
      if (hasOwnerPermissions) {
        val managementUrl = "/banks/" + url(2) + "/accounts/" + url(4) + "/management"
        Some(".navlink [href]" #> { managementUrl } &
        ".navlink *" #> "Counterparties" &
        ".navlink [class+]" #> markIfSelected(managementUrl))
      } else None
    }

    if (url.size > 4) getManagement getOrElse eraseMenu
    else eraseMenu
  }


  // Menu For Entitlements / permissions on an account / view
  def editViews : CssSel = {
    val views = accountJson.flatMap(_.views_available).toList.flatten
    val hasOwnerPermissions = views.exists(v => v.id == Some("owner"))
    
    if(hasOwnerPermissions) {
      if (hasOwnerPermissions) {
        val editViewsUrl = "/banks/" + url(2) + "/accounts/" + url(4) + "/views/list"
        ".navlink [href]" #> { editViewsUrl } &
        ".navlink *" #> "Entitlements" &
        ".navlink [class+]" #> markIfSelected(editViewsUrl)
      } else eraseMenu
    } else eraseMenu
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


  // Menu for which Users have access to which Views
  def privilegeAdmin = {
    val url = S.uri.split("/", 0)

    def getPrivilegeAdmin = {
      val views = accountJson.flatMap(_.views_available).toList.flatten
      //TODO: Determine this in a better way
      val hasOwnerPermissions = views.exists(v => v.id == Some("owner"))
      
      if (hasOwnerPermissions) {
        val permissionsUrls = "/permissions/banks/" + url(2) + "/accounts/" + url(4)
        Some(".navitem *" #> {
        ".navlink [href]" #> permissionsUrls &
          ".navlink *" #> "Users" &
          ".navlink [class+]" #> markIfSelected(permissionsUrls)
        })
      } else None
    }
    
    def hide = ".navitem *" #> ""

    if (url.size > 4) getPrivilegeAdmin.getOrElse(hide) 
    else hide
  }

  def markIfSelected(href: String): Box[String] = {
    val currentHref = S.uri
    if (href.equals(currentHref)) Full("selected")
    else Empty
  }

  def listAccounts = {
    val banksJsonBox = ObpAPI.allBanks
    val bankJsons : List[BankJson] = banksJsonBox.map(_.bankJsons).toList.flatten
    val accountJsons : List[BarebonesAccountJson] = bankJsons.flatMap(bankJson => {
      if(OAuthClient.loggedIn) {
        ObpAPI.privateAccounts(bankJson.id.getOrElse("")).flatMap(_.accounts)
      } else {
        ObpAPI.publicAccounts(bankJson.id.getOrElse("")).flatMap(_.accounts)
      }
    }).flatten
    
    val availableAccounts: List[(String, String)] = ("0", "--> Choose an account") :: {
      accountJsons.map(accountJson => {
        val bankId = accountJson.bank_id.getOrElse("")
        val accountId = accountJson.id.getOrElse("")
        val label = accountJson.label.getOrElse("")
        
        ({bankId + "," + accountId}, {bankId + " - " + label})
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
          S.redirectTo("/banks/" + bankId + "/accounts/" + accountId + "/public")
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
    }
  }
}