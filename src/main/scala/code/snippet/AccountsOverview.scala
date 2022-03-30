/**
Open Bank Project - Sofi Web Application
Copyright (C) 2011 - 2021, TESOBE GmbH.

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

 */

package code.snippet

import java.util.{Calendar, Date}

import code.lib.ObpAPI.{DESC, dateFormat}
import code.lib.ObpJson._
import code.lib.{Header, OAuthClient, ObpAPI}
import code.util.Helper.MdcLoggable
import code.util.Util
import net.liftweb.http.js.JE.JsRaw
import net.liftweb.http.js.JsCmds.Noop
import net.liftweb.http.{S, SHtml}
import net.liftweb.util.Helpers._
import net.liftweb.util.Props

import scala.xml.Text

class AccountsOverview extends MdcLoggable {

  //val banksJsonBox = ObpAPI.allBanks
  //val bankJsons : List[BankJson] = banksJsonBox.map(_.bankJsons).toList.flatten.distinct

  /*val bankIds = for {
    bank <- bankJsons
    id <- bank.id
  } yield id */

  //logger.info("Accounts Overview: Bank ids found: " + bankIds)

  type BankID = String
  val publicAccountJsons : List[(BankID, BarebonesAccountJson)] = for {
    publicAccountsJson <- ObpAPI.publicAccounts.toList
    barebonesAccountJson <- publicAccountsJson.accounts.toList.flatten
    bankId <- barebonesAccountJson.bank_id
  } yield (bankId, barebonesAccountJson)

  logger.debug("Accounts Overview: Public accounts found: " + publicAccountJsons)

  val privateAccountJsons : List[(BankID, BarebonesAccountJson, AccountsBalancesJsonV400)] = for {
    privateAccountsJson <- ObpAPI.privateAccounts.toList
    barebonesAccountJson <- privateAccountsJson.accounts.toList.flatten
    bankId <- barebonesAccountJson.bank_id
    balances <- ObpAPI.getAccountBalances(bankId)
  } yield (bankId, barebonesAccountJson, balances)

  logger.debug("Accounts Overview: Private accounts found: " + privateAccountJsons)
  
  def calculateExpenditureOverviewForLoggedUser(monthsAgo: Int): List[Map[String, Double]] = {

    val transactionsForAccount : List[(BankID, BarebonesAccountJson, AccountsBalancesJsonV400, TransactionsJson)] = for {
      privateAccountsJson <- ObpAPI.privateAccounts.toList // Get all accounts at all banks for logged on user
      accountJson: BarebonesAccountJson <- privateAccountsJson.accounts.toList.flatten
      bankId <- accountJson.bank_id
      balances <- ObpAPI.getAccountBalances(bankId)
      accountId <- accountJson.id
      transactionsJson: TransactionsJson <- ObpAPI.transactions(bankId, accountId, "owner", Some(10000), None, Some(Util.monthsAgo(monthsAgo)), Some(now), Some(DESC))
    } yield (bankId, accountJson, balances, transactionsJson)
    
    val expenditureOverviews = transactionsForAccount map { x =>
      val transactionsJson: List[TransactionJson] = x._4.transactions.getOrElse(Nil)
      val tempList: List[(List[TransactionTagJson], Option[TransactionValueJson])] =
        transactionsJson.map(x => (x.tagJsons.getOrElse(Nil), x.details.flatMap(_.value)))
      val expenditures = tempList.filter(i => i._2.map(_.amount).flatten.getOrElse("0").toDouble < 0)
      val categorisedTransactions: List[(String, Option[String], Option[String])] =
        expenditures.map( x =>
          (x._1.flatMap(_.value).headOption.getOrElse("None"),
            x._2.flatMap(_.amount),
            x._2.flatMap(_.currency)
          )
        )
      val groupedTransactionsByCategoryAndCurrency: Map[String, List[(String, Option[String], Option[String])]] = categorisedTransactions.groupBy(i => i._3.getOrElse("Missing currency") + "::" + i._1)
      val sumByCategory: Map[String, Double] = groupedTransactionsByCategoryAndCurrency.map(i => (i._1, i._2.map(_._2.getOrElse("0").toDouble).sum))
      sumByCategory
    }
    logger.debug(expenditureOverviews)
    expenditureOverviews
  }
  

  /*

      ".resource" #> resources.map { i =>
      ".content-box__headline *" #> i.summary &
      ".resource_summary [href]" #> s"#${i.id}" &

   */

  def shouldShowPublicAccounts = {
    if(Props.get("hide_public_accounts_panel", "false").toBoolean) {
      "#public_account_info_box [style]" #> "display:none"
    } else {
      "#public_account_info_box [style]" #> "display:block"
    }
  }
  def ssoInPreferenceToPublicAccounts = {
    if(Props.get("sso.enabled", "false").toBoolean) {
      logger.debug("Single Sign On is enabled")
      if(OAuthClient.loggedIn) publicAccounts else OAuthClient.redirectToOauthLogin()
    } else {
      publicAccounts
    }
  }
  def publicAccounts = {
    if (publicAccountJsons.size == 0) {
      ".accName *" #> "No public accounts available." &
      ".accLink" #> ""
    } else {
      val sortedPublicAccountJsons = publicAccountJsons.sortBy(a => (a._2.label.toString.toLowerCase, a._2.id.toString.toLowerCase))
      ".accountItem" #> sortedPublicAccountJsons.map {
        case (bankId, accountJson) => {
          //TODO: It might be nice to ensure that the same view is picked each time the page loads
          val views = accountJson.views.toList.flatten
          val aPublicViewId: String = (for {
            aPublicView <- views.filter(view => view.is_public.getOrElse(false)).headOption
            viewId <- aPublicView.id
          } yield viewId).getOrElse("")
          val accountId = accountJson.id.getOrElse("")
          val url = "/banks/" + bankId + "/accounts/" + accountId + "/" + aPublicViewId

          ".accName a *" #> accountDisplayName(accountJson) &
          ".accName a [href]" #> url &
          ".accLink [href]" #> url
        }
      }
    }
  }

  def accountDisplayName(accountJson : BarebonesAccountJson) : String = {
    val label = accountJson.label.getOrElse("")
    if(label != "") label else accountJson.id.getOrElse("unknown account name")
  }

  def authorisedAccounts = {
    def loggedInSnippet = {
      if (privateAccountJsons.size == 0) {
        ".accName *" #> "No authorised accounts available." &
        ".accLink" #> ""
      } else {
        val sortedPrivateAccountJsons = privateAccountJsons.sortBy(_._2.id)
        ".accountItem" #> sortedPrivateAccountJsons.map {
          case (bankId, accountJson, balances) => {
            //TODO: It might be nice to ensure that the same view is picked each time the page loads
            val views = accountJson.views.toList.flatten
            val accountId : String = accountJson.id.getOrElse("")
            val aPrivateViewId: String = (for {
              aPrivateView <- views.filterNot(view => view.is_public.getOrElse(false) || view.short_name.equals(Some("Firehose"))).headOption
              viewId <- aPrivateView.id
            } yield viewId).getOrElse("")
            val url = "/banks/" + bankId + "/accounts/" + accountId + "/" + aPrivateViewId
            val incomeLink = "/banks/" + bankId + "/accounts/" + accountId + "/create-income"
            val expenditureLink = "/banks/" + bankId + "/accounts/" + accountId + "/create-expenditure"
            val dashboardLink = "/banks/" + bankId + "/accounts/" + accountId + "/account-overview-dashboard"

            val balance: String = balances.accounts.filter(_.account_id==accountId)
              .flatMap(_.balances)
              .map(b => b.amount + " " + b.currency)
              .headOption.getOrElse("")
            
            ".balanceValue a *" #> balance &
            ".balanceValue a [href]" #> dashboardLink &
            ".incomeLink [href]" #> incomeLink &
            ".expenditureLink [href]" #> expenditureLink &
            ".accName a *" #> accountDisplayName(accountJson) &
            ".accName a [href]" #> url &
            ".accLink [href]" #> url
          }
        }
      }
    }

    def loggedOutSnippet = {
   //   ".accountItem" #> SHtml.span(Text("S.?("you_are_logged_out")"), Noop,("id","accountsMsg"))
      ".accName *" #> S.?("you_are_logged_out") &
      ".accLink" #> "" &
      ".incomeLink" #> "" &
      ".expenditureLink" #> ""
    }

    if(OAuthClient.loggedIn) loggedInSnippet
    else loggedOutSnippet
  }

  def authorisedAccountsWithManageLinks = {
    def loggedInSnippet = {

      ".accountList" #> privateAccountJsons.map { case (bankId, accountJson, _) => {
        //TODO: It might be nice to ensure that the same view is picked each time the page loads
        val views = accountJson.views.toList.flatten
        val accountId: String = accountJson.id.getOrElse("")
        val aPrivateViewId: String = (for {
          aPrivateView <- views.filterNot(view => view.is_public.getOrElse(false)).headOption
          viewId <- aPrivateView.id
        } yield viewId).getOrElse("")
        val removeAjax = SHtml.ajaxCall(JsRaw("this.getAttribute('data-accountid')"), accountId => {
          ObpAPI.deleteAccount(bankId, accountId)
          Noop
        })

        ".accLink *" #> accountDisplayName(accountJson) &
          ".accLink [href]" #> {
            "/banks/" + bankId + "/accounts/" + accountId + "/" + aPrivateViewId
          } &
          ".remove [data-bankid]" #> bankId &
          ".remove [data-accountid]" #> accountId &
          ".remove [onclick]" #> removeAjax
      }
      }
    }
    def loggedOutSnippet = {
      ".accountList" #> SHtml.span(Text(S.?("you_are_logged_out")), Noop,("id","accountsMsg"))
    }

    if(OAuthClient.loggedIn) loggedInSnippet
    else loggedOutSnippet
  }


  def authorisedAccountsDashboard = {
    def loggedInSnippet = {

      ".account" #> privateAccountJsons.map {case (bankId, accountJson, _) => {
        //TODO: It might be nice to ensure that the same view is picked each time the page loads
        val views = accountJson.views.toList.flatten
        val accountId : String = accountJson.id.getOrElse("")
        val aPrivateViewId: String = (for {
          aPrivateView <- views.filterNot(view => view.is_public.getOrElse(false)).headOption
          viewId <- aPrivateView.id
        } yield viewId).getOrElse("")

        ".account *" #> accountDisplayName(accountJson) &
        ".account [value]" #> {
          "/dashboard/banks/" + bankId + "/accounts/" + accountId + "/" + aPrivateViewId + "/banks/" + bankId + "/accounts/" + accountId + "/" + aPrivateViewId
        }
      }}
    }

    def loggedOutSnippet = {
      ".accountList" #> SHtml.span(Text(S.?("you_are_logged_out")), Noop,("id","accountsMsg"))
    }

    if(OAuthClient.loggedIn) loggedInSnippet
    else loggedOutSnippet
  }


}
