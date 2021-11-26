/**
 * Open Bank Project - Sofi Web Application
 * Copyright (C) 2011 - 2021, TESOBE GmbH
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
 * TESOBE GmbH.
 * Osloer Str. 16/17
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

import code.Constant._
import net.liftweb.http.S
import net.liftweb.http.LiftRules
import net.liftweb.util.Helpers._
import net.liftweb.sitemap.Loc
import net.liftweb.common.Box
import net.liftweb.common.Full
import net.liftweb.common.Empty
import net.liftweb.sitemap.SiteMapSingleton
import code.lib.ObpJson._
import code.lib.ObpAPI
import code.util.Helper
import net.liftweb.util.{CssSel, Props}

import scala.xml.NodeSeq


class Nav {

  val url = S.uri.split("/", 0)
  val accountJson : Option[AccountJson]= {
    if (url.size > 4) {

      val viewId = CUSTOM_OWNER_VIEW_ID //if we can't access the owner view, account returns nothing
      val bankId = url( url.indexOf("banks")+1 )
      val accountId = url( url.indexOf("accounts")+1 )

      ObpAPI.getAccount(bankId, accountId, viewId)
    } else {
      None
    }
  }
  val viewJsons: List[ViewJson] = {
    accountJson.flatMap(accJson => {
      accJson.views_available
    }).toList.flatten
  }
  val hasManagementAccess = accountJson match {
    case Some(x) => Helper.hasManagementAccess(x)
    case _ => true
  }


  def eraseMenu =
    "*" #> ""


  def views: net.liftweb.util.CssSel = {
    val allowedSystemViews: List[String] = Props.get("sytems_views_to_display", "owner,accountant,auditor")
      .split(",").map(_.trim()).toList
    val url = S.uri.split("/", 0)
    if (url.size > 4) {
      if (viewJsons.size == 0) {
        eraseMenu
      } else {
        ".navitem *" #> {
          viewJsons.filter(i => allowedSystemViews.exists(i.id.getOrElse("").toLowerCase == _.toLowerCase) || i.id.getOrElse("").startsWith("_")).map(viewJson => {
            val viewUrl = "/banks/" + url(2) + "/accounts/" + url(4) + "/" + viewJson.id.getOrElse("")
            ".navlink [href]" #> viewUrl &
            ".navlink *" #> viewJson.short_name.getOrElse("") &
            ".navlink [class+]" #> markIfSelected(viewUrl)
          })
        }
      }
    } else
      eraseMenu
  }


  def navAccountSettings : CssSel = {
    if (hasManagementAccess) {
      "nosuchtag" #> "" // there doesn't seem to be a noop?
    } else {
      eraseMenu
    }
  }


  def management = {
    
    val url = S.uri.split("/", 0)

    // Menu for a page which lists counterparties and their metadata (and edits the metadata)
    def getManagement = {
      if (hasManagementAccess && Props.get("management.counterparties.enabled", "true").toBoolean) {
        val managementUrl = "/banks/" + url(2) + "/accounts/" + url(4) + "/management"
        Some(".navlink [href]" #> { managementUrl } &
        ".navlink *" #> "Counterparties" &
        ".navlink [class+]" #> markIfSelected(managementUrl))
      } else None
    }

    if (url.size > 4) getManagement getOrElse eraseMenu
    else eraseMenu
  }
  
  // Menu For home page
  def homePage : CssSel = {
    val homePageUrl = Props.get("base_url").getOrElse("unknown")
    ".navlink [href]" #> { homePageUrl } &
      ".navlink *" #> "Home"
  }
  
  // Menu For Entitlements / permissions on an account / view
  def editViews : CssSel = {
    if (hasManagementAccess &&
      Props.get("management.views.enabled", "true").toBoolean &&
      url.size == 6 && url(1) == "banks" && url(3) == "accounts") {
      val editViewsUrl = "/banks/" + url(2) + "/accounts/" + url(4) + "/views/list"
      ".navlink [href]" #> { editViewsUrl } &
      ".navlink *" #> "Views" &
      ".navlink [class+]" #> markIfSelected(editViewsUrl)
    } else eraseMenu
  }

  // Menu for settings on account
  def accountSettings : CssSel = {
    if (hasManagementAccess && url.size == 6 && url(1) == "banks" && url(3) == "accounts") {
      val accountSettingsURL = "/banks/" + url(2) + "/accounts/" + url(4) + "/settings"
      ".navlink [href]" #> { accountSettingsURL } &
      ".navlink *" #> "Settings" &
      ".navlink [class+]" #> markIfSelected(accountSettingsURL)
    } else eraseMenu
  }
  // Menu for adding an income
  def incomeSettings : CssSel = {
    if (hasManagementAccess && url.size == 6 && url(1) == "banks" && url(3) == "accounts") {
      val addIncomeUrl = "/banks/" + url(2) + "/accounts/" + url(4) + "/create-income"
      ".navlink [href]" #> { addIncomeUrl } &
      ".navlink *" #> "Create income" &
      ".navlink [class+]" #> markIfSelected(addIncomeUrl)
    } else eraseMenu
  }
  // Menu for adding an income
  def outcomeSettings : CssSel = {
    if (hasManagementAccess && url.size == 6 && url(1) == "banks" && url(3) == "accounts") {
      val addIncomeUrl = "/banks/" + url(2) + "/accounts/" + url(4) + "/create-expenditure"
      ".navlink [href]" #> { addIncomeUrl } &
      ".navlink *" #> "Create expenditure" &
      ".navlink [class+]" #> markIfSelected(addIncomeUrl)
    } else eraseMenu
  }

  def createAccount : CssSel = {
    val bankId: String = "user." + ObpAPI.currentUser.map(_.user_id).getOrElse(System.currentTimeMillis())
    if (hasManagementAccess) {
      val accountSettingsURL = "/banks/" + bankId + "/accounts/create-bank-account"
      ".navlink [href]" #> { accountSettingsURL } &
        ".navlink *" #> "Create account" &
        ".navlink [class+]" #> markIfSelected(accountSettingsURL)
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
      if (hasManagementAccess && Props.get("management.users.enabled", "true").toBoolean) {
        val permissionsUrls = "/banks/" + url(2) + "/accounts/" + url(4) + "/permissions"
        Some(".navitem *" #> {
        ".navlink [href]" #> permissionsUrls &
          ".navlink *" #> "Users" &
          ".navlink [class+]" #> markIfSelected(permissionsUrls)
        })
      } else None
    }
    
    if (url.size > 4) getPrivilegeAdmin.getOrElse(eraseMenu) 
    else eraseMenu
  }

  def markIfSelected(href: String): Box[String] = {
    val currentHref = S.uri
    if (href.equals(currentHref)) Full("selected")
    else Empty
  }
}
