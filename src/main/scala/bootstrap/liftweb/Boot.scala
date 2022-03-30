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
package bootstrap.liftweb

import java.io.{File, FileInputStream}
import java.util.Locale

import code.Constant._
import code.lib.ObpJson._
import code.lib.{OAuthClient, ObpAPI, ObpGet}
import code.snippet._
import code.util.Helper.MdcLoggable
import code.util.{Helper, MyExceptionLogger, Util}
import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.http.provider.HTTPCookie
import net.liftweb.sitemap.Loc._
import net.liftweb.sitemap._
import net.liftweb.util.Helpers._
import net.liftweb.util._

/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */
class Boot extends MdcLoggable{
  def boot {

    MDC.put( ("host", Helper.getHostname()) )

    val runningMode = Props.mode match {
      case Props.RunModes.Production => "Production mode"
      case Props.RunModes.Staging => "Staging mode"
      case Props.RunModes.Development => "Development mode"
      case Props.RunModes.Test => "test mode"
      case _ => "other mode"
    }

    logger.info("running mode: " + runningMode)

    val contextPath = LiftRules.context.path
    val propsPath = tryo{Box.legacyNullTest(System.getProperty("props.resource.dir"))}.toList.flatten

    logger.info("external props folder: " + propsPath)

    /**
     * Where this application looks for props files:
     *
     * All properties files follow the standard lift naming scheme for order of preference (see https://www.assembla.com/wiki/show/liftweb/Properties)
     * within a directory.
     *
     * The first choice of directory is $props.resource.dir/CONTEXT_PATH where $props.resource.dir is the java option set via -Dprops.resource.dir=...
     * The second choice of directory is $props.resource.dir
     *
     * For example, on a production system:
     *
     * thing1.example.com with context path /thing1
     *
     * Looks first in (outside of war file): $props.resource.dir/thing1, following the normal lift naming rules (e.g. production.default.props)
     * Looks second in (outside of war file): $props.resource.dir, following the normal lift naming rules (e.g. production.default.props)
     * Looks third in the war file
     *
     * and
     *
     * thing2.example.com with context path /thing2
     *
     * Looks first in (outside of war file): $props.resource.dir/thing2 , following the normal lift naming rules (e.g. production.default.props)
     * Looks second in (outside of war file): $props.resource.dir, following the normal lift naming rules (e.g. production.default.props)
     * Looks third in the war file, following the normal lift naming rules
     *
     */

    val firstChoicePropsDir = for {
      propsPath <- propsPath
    } yield {
      Props.toTry.map {
        f => {
          val name = propsPath + contextPath + f() + "props"
          name -> { () => tryo{new FileInputStream(new File(name))} }
        }
      }
    }

    val secondChoicePropsDir = for {
      propsPath <- propsPath
    } yield {
      Props.toTry.map {
        f => {
          val name = propsPath +  f() + "props"
          name -> { () => tryo{new FileInputStream(new File(name))} }
        }
      }
    }

    Props.whereToLook = () => {
      firstChoicePropsDir.toList.flatten ::: secondChoicePropsDir.toList.flatten
    }

    if(Props.get("defaultAuthProvider").isEmpty) {
      throw new Exception("defaultAuthProvider must be specified in the props file!")
    }

    def check(bool: Boolean) : Box[LiftResponse] = {
      if(bool){
        Empty
      }else{
        Full(PlainTextResponse("unauthorized"))
      }
    }

    def logOrReturnResult[T](result : Box[T]) : Box[T] = {
      result match {
        case Failure(msg, _, _) => logger.warn("Problem getting url " + tryo{S.uri} + ": " + msg)
        case _ => //do nothing
      }
      result
    }

    //getTransactionsAndView can be called twice by lift, so it's best to memoize the result of the potentially expensive calculation
    object transactionsMemo extends RequestVar[Box[Box[((TransactionsJson, AccountJson, TransactionsListURLParams))]]](Empty)

    def getTransactions(URLParameters: List[String]): Box[(TransactionsJson, AccountJson, TransactionsListURLParams)] =
      {

        def calculateTransactions() = {
          val bankId = URLParameters(0)
          val accountId = URLParameters(1)
          val viewId = URLParameters(2)

          val transactionsURLParams = TransactionsListURLParams(bankId = bankId, accountId = accountId, viewId = viewId)

          val result = logOrReturnResult {

            for {
              //TODO: Pagination: This is not totally trivial, since the transaction list groups by date and 2 pages may have some transactions on the same date
              transactionsJson <- ObpAPI.transactions(bankId, accountId, viewId, Some(10000), Some(0), None, Some(now), None)
              accountJson <- ObpAPI.getAccount(bankId, accountId, viewId) //TODO: Execute this request and the one above in parallel
            } yield {
              (transactionsJson, accountJson, transactionsURLParams)
            }

          }

          transactionsMemo.set(Full(result))
          result
        }

        transactionsMemo.get match {
          case Full(something) => something
          case _ => calculateTransactions()
        }

      }

    //getDashboard can be called twice by lift, so it's best to memoize the result of the potentially expensive calculation
    object dashboardMemo extends RequestVar[Box[Box[((TransactionsJson, AccountJson, TransactionsListURLParams, TransactionsJson, AccountJson, TransactionsListURLParams))]]](Empty)

    def getDashboard(URLParameters: List[String]): Box[(TransactionsJson, AccountJson, TransactionsListURLParams, TransactionsJson, AccountJson, TransactionsListURLParams)] =
      {

        def calculateTransactions() = {
          val bankId1 = URLParameters(0)
          val accountId1 = URLParameters(1)
          val viewId1 = URLParameters(2)

          val bankId2 = URLParameters(3)
          val accountId2 = URLParameters(4)
          val viewId2 = URLParameters(5)

          val transactions1URLParams = TransactionsListURLParams(bankId = bankId1, accountId = accountId1, viewId = viewId1)
          val transactions2URLParams = TransactionsListURLParams(bankId = bankId2, accountId = accountId2, viewId = viewId2)

          val result = logOrReturnResult {

            for {
              //TODO: Pagination: This is not totally trivial, since the transaction list groups by date and 2 pages may have some transactions on the same date
              transactions1Json <- ObpAPI.transactions(bankId1, accountId1, viewId1, Some(10000), Some(0), None, None, None)
              account1Json <- ObpAPI.getAccount(bankId1, accountId1, viewId1) //TODO: Execute this request and the one above in parallel

              transactions2Json <- ObpAPI.transactions(bankId2, accountId2, viewId2, Some(10000), Some(0), None, None, None)
              account2Json <- ObpAPI.getAccount(bankId2, accountId2, viewId2) //TODO: Execute this request and the one above in parallel
            } yield {
              (transactions1Json, account1Json, transactions1URLParams, transactions2Json, account2Json, transactions2URLParams)
            }

          }

          dashboardMemo.set(Full(result))
          result
        }

        dashboardMemo.get match {
          case Full(something) => something
          case _ => calculateTransactions()
        }

      }

    //getAccount can be called twice by lift, so it's best to memoize the result of the potentially expensive calculation
    object accountMemo extends RequestVar[Box[Box[(code.lib.ObpJson.OtherAccountsJson, code.snippet.ManagementURLParams)]]](Empty)

    def getAccount(URLParameters : List[String]) =
    {
        def calculateAccount() = {
          val bankUrl = URLParameters(0)
          val accountUrl = URLParameters(1)

          val urlParams = ManagementURLParams(bankUrl, accountUrl)

          val result = logOrReturnResult {
            for {
              otherAccountsJson <- ObpGet(s"/$versionOfApi/banks/" + bankUrl + "/accounts/" + accountUrl + "/owner/" + "other_accounts").flatMap(x => x.extractOpt[OtherAccountsJson])
            } yield (otherAccountsJson, urlParams)
          }

          accountMemo.set(Full(result))
          result
        }

      accountMemo.get match {
        case Full(something) => something
        case _ => calculateAccount()
      }
    }


    def getAccounts(URLParameters : List[String]): Box[List[BarebonesAccountJson]] =
    {
      val result =
        for {
          accountJson <- ObpGet(s"/$versionOfApi/accounts/private").flatMap(_.extractOpt[BarebonesAccountsJson])
        } yield (accountJson.accounts)

      result match {
        case Full(s) => s
        case _ => None
      }
    }
    
    def getBanks(URLParameters : List[String]): Box[List[BankJson400]] = {
      if (URLParameters.length == 2) {
        val bank = URLParameters(0)
      }
      val result =
        for {
          json <- ObpGet(s"/$versionOfApi/banks").flatMap(_.extractOpt[BanksJson400])
        } yield (json.banks)

      result
    }

    //getTransaction can be called twice by lift, so it's best to memoize the result of the potentially expensive calculation
    object transactionMemo extends RequestVar[Box[Box[(code.lib.ObpJson.TransactionJson, code.lib.ObpJson.AccountJson, code.snippet.CommentsURLParams)]]](Empty)
    def getTransaction(URLParameters: List[String]) =
      {

        def calculateTransaction() = {
          if (URLParameters.length == 4) {
            val bank = URLParameters(0)
            val account = URLParameters(1)
            val transactionID = URLParameters(2)
            val viewName = URLParameters(3)

            val transactionJson = ObpGet(s"/$versionOfApi/banks/" + bank + "/accounts/" + account + "/" + viewName +
              "/transactions/" + transactionID + "/transaction").flatMap(x => x.extractOpt[TransactionJson])

            val commentsURLParams = CommentsURLParams(bankId = bank, accountId = account, transactionId = transactionID, viewId = viewName)

            val result = logOrReturnResult {
              for {
                tJson <- transactionJson
                accountJson <- ObpAPI.getAccount(bank, account, viewName)
              } yield (tJson, accountJson, commentsURLParams)
            }

            transactionMemo.set(Full(result))
            result
          } else
            Empty
        }

        transactionMemo.get match {
          case Full(something) => something
          case _ => calculateTransaction()
        }
      }

    def getAccountViewsAndPermission(URLParameters: List[String]): Box[(List[ViewJson], AccountJson, PermissionsUrlParams)] = {
      if (URLParameters.length == 2) {
        val bank = URLParameters(0)
        val account = URLParameters(1)

        logOrReturnResult {
          for {
            viewsJson <- ObpAPI.getViews(bank, account)
            accountJson <- ObpAPI.getAccount(bank, account, CUSTOM_OWNER_VIEW_ID) //TODO: Execute this request and the one above in parallel
          } yield {
            (viewsJson, accountJson, PermissionsUrlParams(bank, account))
          }

        }
      } else Empty
    }

    def getAccountViews(URLParameters: List[String]): Box[(List[ViewJson])] = {
      if (URLParameters.length == 2) {
        val bank = URLParameters(0)
        val account = URLParameters(1)

        logOrReturnResult {
          for {
            viewsJson <- ObpAPI.getViews(bank, account)
          } yield {
            viewsJson
          }

        }
      } else Empty
    }

    def getCompleteAccountViews(URLParameters: List[String]): Box[ViewsDataJSON] = {
      if (URLParameters.length == 2) {
        val bank = URLParameters(0)
        val account = URLParameters(1)

        logOrReturnResult {
          for {
            viewsJson <- ObpAPI.getCompleteViews(bank, account)
          } yield {
            ViewsDataJSON(viewsJson, bank,account)
          }

        }
      } else Empty
    }

    def getPermissions(URLParameters: List[String]): Box[(PermissionsJson, AccountJson, List[ViewJson], PermissionsUrlParams)] = {
      if (URLParameters.length == 2) {
        val bank = URLParameters(0)
        val account = URLParameters(1)

        logOrReturnResult {
          for {
            permissionsJson <- ObpAPI.getPermissions(bank, account)
            accountViewsJson <- ObpAPI.getViews(bank, account)
            accountJson <- ObpAPI.getAccount(bank, account, CUSTOM_OWNER_VIEW_ID) //TODO: Execute this request and the one above in parallel
          } yield (permissionsJson, accountJson, accountViewsJson, PermissionsUrlParams(bank, account))
        }
      } else Empty
    }

    // Have this to be able to 404 if user not owner
    def getAccountSettings(URLParameters: List[String]): Box[List[String]] = {
      if (URLParameters.length == 2) {
        val bank = URLParameters(0)
        val account = URLParameters(1)

        logOrReturnResult {
          for {
            _ <- ObpAPI.getAccount(bank, account, CUSTOM_OWNER_VIEW_ID)
          } yield {
            URLParameters
          }
        }
      } else Empty
    }

    def getDashboardAccountOverview(URLParameters: List[String]): Box[List[String]] = {
      if (URLParameters.length == 2) {
        logOrReturnResult {
          Full(URLParameters)
        }
      } else Empty
    }
    

    // Build SiteMap
    // Note: See Nav.scala which modifies the menu
    // Note: This might be worth looking at: https://github.com/dph01/lift-TBUtils

    val sitemap = List(
      Menu.i("Home") / "index",
      Menu.i("Correlated-user") / "correlated-user" >> EarlyResponse(() => {
        setCorrelatedCookie
      }),
      Menu.i("About") / "about",
      Menu.i("404") / "404" >> Hidden,

      //Menu.i("Define Dashboard") / "dd",

      Menu.i("OAuth Callback") / "oauthcallback" >> Hidden >> EarlyResponse(() => {
        OAuthClient.handleCallback()
      }),
      //test if the bank exists and if the user has access to the management page
      Menu.params[(OtherAccountsJson, ManagementURLParams)]("Management", "management", getAccount _ , t => List("")) / "banks" / * / "accounts" / * / "management",

      Menu.params[List[String]]("AccountSettings", "Account settings", getAccountSettings _, x => List("")) / "banks" / * / "accounts" / * / "settings",
      
      Menu.params[List[BankJson400]]("CreateBankAccount", "Create bank account", getBanks _, x => List("")) / "banks" / * / "accounts" / "create-bank-account",
      
      Menu.params[List[String]]("DashboardAccountOverview", "Dashboard Account Overview", getDashboardAccountOverview _, x => List("")) / "banks" / * / "accounts"  / * /  "account-overview-dashboard",
      Menu.params[List[String]]("CreateIncome", "Create Income", getAccountSettings _, x => List("")) / "banks" / * / "accounts"  / * /  "create-income",
      Menu.params[List[String]]("CreateExpenditure", "Create Expenditure", getAccountSettings _, x => List("")) / "banks" / * / "accounts"  / * /  "create-expenditure",

      Menu.params[ViewsDataJSON]("Views","Views Overview", getCompleteAccountViews _ , x => List("")) / "banks" / * / "accounts" / * / "views" / "list",


      Menu.params[(List[ViewJson], AccountJson, PermissionsUrlParams)]("Create Permission", "create permissions", getAccountViewsAndPermission _ , x => List(""))
      / "banks" / * / "accounts" / * / "permissions" / "create" ,

      Menu.params[(PermissionsJson, AccountJson, List[ViewJson], PermissionsUrlParams)]("Permissions", "permissions", getPermissions _ , x => List("")) / "banks" / * / "accounts" / * / "permissions" ,
      Menu.params[List[BarebonesAccountJson]]("Manage Accounts", "manage accounts", getAccounts _ , x => List("")) / "list-accounts" ,

      Menu.params[(TransactionsJson, AccountJson, TransactionsListURLParams)]("Transactions", "Transactions", getTransactions _ ,  t => List("") )
      / "banks" / * / "accounts" / * / *,

      Menu.params[(TransactionJson, AccountJson, CommentsURLParams)]("Transaction", "Transaction Detail", getTransaction _ ,  t => List("") )
      / "banks" / * / "accounts" / * / "transactions" / * / *,

      Menu.params[(TransactionsJson, AccountJson, TransactionsListURLParams, TransactionsJson, AccountJson, TransactionsListURLParams)]("Dashboard", "dashboard", getDashboard _ ,  t => List("") )
      / "dashboard" / "banks" / * / "accounts" / * / * / "banks" / * / "accounts" / * / *

//      Menu.params[(TransactionsJson, AccountJson, TransactionsListURLParams, TransactionsJson, AccountJson, TransactionsListURLParams)]("Dashboard", "dashboard", getDashboard _ ,  t => List("") )
//      / "dashboard" / "banks" / * / "accounts" / * / *,

//      Menu.params[()]("Dashboard", "dashboard", getDashboard _ ,  t => List("") )
//      / "dashboard"



    )

    LiftRules.setSiteMap(SiteMap.build(sitemap.toArray))



    LiftRules.addToPackages("code")

    // Use jQuery from js directory
    LiftRules.jsArtifacts = net.liftweb.http.js.jquery.JQueryArtifacts

    //Show the spinny image when an Ajax call starts
    LiftRules.ajaxStart =
      Full(() => LiftRules.jsArtifacts.show("ajax-loader").cmd)

    // Make the spinny image go away when it ends
    LiftRules.ajaxEnd =
      Full(() => LiftRules.jsArtifacts.hide("ajax-loader").cmd)

    // Force the request to be UTF-8
    LiftRules.early.append(_.setCharacterEncoding("UTF-8"))

    //set base localization according to a props value (instead of computer default)
    val locale = Locale.getAvailableLocales().toList.filter { l =>
      l.toLanguageTag == Props.get("language_tag", "en-GB")
    }.head
    Locale.setDefault(locale)
    logger.info("Default Project Locale is :" + locale)
    
    // Properly convert a language tag to a Locale
    def computeLocale(tag : String) = tag.split(Array('-', '_')) match {
      case Array(lang) => new Locale(lang)
      case Array(lang, country) => new Locale(lang, country)
      case Array(lang, country, variant) => new Locale(lang, country, variant)
    }
    // Cookie name
    val localeCookieName = "SELECTED_LOCALE"
    LiftRules.localeCalculator = {
      case fullReq @ Full(req) => {
        // Check against a set cookie, or the locale sent in the request 
        def currentLocale : Locale = {
          S.findCookie(localeCookieName).flatMap {
            cookie => cookie.value.map(computeLocale)
          } openOr locale
        }
        
        // Check to see if the user explicitly requests a new locale 
        S.param("locale") match {
          case Full(requestedLocale) if requestedLocale != null => {
            val computedLocale = computeLocale(requestedLocale)
            S.addCookie(HTTPCookie(localeCookieName, requestedLocale))
            computedLocale
          }
          case _ => currentLocale
        }
      }
      case _ => locale
    }

    // LiftRules.resourceNames = "i18n.lift-core" :: Nil

    // Use HTML5 for rendering
    LiftRules.htmlProperties.default.set((r: Req) =>
      new Html5Properties(r.userAgent))

    LiftRules.explicitlyParsedSuffixes = Helpers.knownSuffixes &~ (Set("com"))

    LiftRules.exceptionHandler.prepend{
      case MyExceptionLogger(_, _, t) => throw t // this will never happen
    }



    LiftRules.uriNotFound.prepend(NamedPF("404handler"){ 
      case (req,failure) =>
        if(!OAuthClient.loggedIn) // Force logon. This app does not provide functionalities in other case anyway.
          OAuthClient.redirectToOauthLogin()
        else // If user is logged on and path is still invalid show the not found page
          NotFoundAsTemplate(ParsePath(List("404"),"html",true,false)) 
    })

  }

  private def setCorrelatedCookie = {
    S.param("correlated_user_id") match {
      case Full(correlatedUserId) if correlatedUserId != null => {
        // Clean up cookies
        S.deleteCookie(correlatedCustomerIdCreatedCookieName)
        S.deleteCookie(linkBetweenCorrelatedUserAndCustomerCreatedCookieName)
        S.deleteCookie(correlatedUserIdBoundCookieName)
        // Set the cookie
        S.addCookie(HTTPCookie(correlatedUserIdTargetCookieName, correlatedUserId))
        Util.correlatedUserFlow(correlatedUserId)
        S.redirectTo("/")
      }
      case _ => S.redirectTo("/")
    }
  }
}
