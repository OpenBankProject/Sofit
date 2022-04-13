package code.snippet

import _root_.net.liftweb.http.js.JsCmd
import code.Constant._
import code.lib.ObpAPI.{DESC, getAccount}
import code.lib.ObpJson.{TransactionJson, TransactionTagJson, TransactionValueJson, TransactionsJson}
import code.lib.{OAuthClient, ObpAPI, ObpJson}
import code.util.Helper.{MdcLoggable, getAccountTitle}
import code.util.Util
import net.liftweb.common.Box
import net.liftweb.http.js.JE.JsRaw
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.{RequestVar, S, SHtml}
import net.liftweb.util.Helpers._

import scala.xml.NodeSeq


class DashboardAccountOverview(params: List[String]) extends MdcLoggable {
  private object monthsAgoVar extends RequestVar("1")
  val listOfTags: Seq[(String, String)] = List(("1","1 month"), ("2","2 months"), ("3","3 months"), ("4","4 months"), 
    ("5","5 months"), ("6","6 months"), ("7","7 months"), ("8","8 months"), 
    ("9","9 months"), ("10","10 months"), ("11","11 months"), ("12","12 months"))
  val bankId = params(0)
  val accountId = params(1)
  lazy val accountJson = getAccount(bankId, accountId, CUSTOM_OWNER_VIEW_ID).openOrThrowException("Could not open accountJson")
  def accountTitle = {
    if(OAuthClient.loggedIn) {
      ".account-title  a *" #> getAccountTitle(accountJson) &
      ".account-title a [href]" #> "owner" 
    } else { 
      OAuthClient.redirectToOauthLogin() 
    }
  }
  
  def calculateExpenditureOverview(xhtml: NodeSeq): NodeSeq = {
    if(OAuthClient.loggedIn) calculateExpenditureOverviewCommon(xhtml) else OAuthClient.redirectToOauthLogin()
  }

  //set up ajax handlers to edit account label
  def calculateExpenditureOverviewCommon(xhtml: NodeSeq): NodeSeq = {
    
    def process(): JsCmd = {
      val expenditures: List[(String, Double)] = calculateTransactionOverview(bankId, accountId, false,monthsAgoVar.is.toInt).map(_.toList)
        .toList.flatten.sortBy(_._2)
      val expenditureData = expenditures.map(i => s"['${i._1}',${i._2.abs}, '#EB6028']").mkString(",")
      val incomes: List[(String, Double)] = calculateTransactionOverview(bankId, accountId, true,monthsAgoVar.is.toInt).map(_.toList)
        .toList.flatten.sortBy(_._2)
      val incomeMaxValue = incomes.map(_._2).max.abs
      val incomeData = incomes.map(i => s"['${i._1}',${i._2.abs}, '#50B165']").mkString(",")
      
      val script = s"""google.charts.load('current', {'packages':['corechart']});
                     |google.charts.setOnLoadCallback(drawChart);
                     |
                     |function drawChart() {
                     |var expenditureData = google.visualization.arrayToDataTable([
                     |  ['Category', '€', { role: 'style' }],
                     |  $expenditureData
                     |]);
                     |
                     |var expenditureOptions = {
                     |  title:'Expenditure',
                     |  legend: { position: "none" },
                     |};
                     |
                     |var expenditureChart = new google.visualization.BarChart(document.getElementById('expenditureChart'));
                     |  expenditureChart.draw(expenditureData, expenditureOptions);
                     |  
                     |var incomeData = google.visualization.arrayToDataTable([
                     |  ['Category', '€', { role: 'style' }],
                     |  $incomeData
                     |]);
                     |
                     |var incomeOptions = {
                     |  title:'Income',
                     |  legend: { position: "none" },
                     |};
                     |
                     |var incomeChart = new google.visualization.BarChart(document.getElementById('incomeChart'));
                     |  incomeChart.draw(incomeData, incomeOptions);
                     |  
                     |}""".stripMargin
      
      JsRaw(script)
    }
    
    val balances: Box[ObpJson.AccountBalanceJsonV400] = ObpAPI.getAccountBalances(bankId, accountId)
    val (amount, currency) = balances.flatMap(_.balances.headOption).map(i => (i.amount, i.currency)).getOrElse(("", ""))
    val customers: List[ObpJson.CustomerWithAttributesJsonV300] = ObpAPI.getCustomersForCurrentUser().map(_.customers).getOrElse(Nil)
    val creditRating = customers.flatMap(_.credit_rating.map(_.rating)).headOption.getOrElse("0")
    val profileCompleteness: String = customers.flatMap(_.customer_attributes
      .filter(_.name == "CREDIT_SCORE_READINESS")).map(_.value).headOption.getOrElse("0")

    val creditRatingToInt = tryo(creditRating.toInt).getOrElse(0)
    val profileCompletenessToInt = tryo(profileCompleteness.toInt).getOrElse(0)
    val compared: Int = tryo(amount.toDouble).map(BigDecimal(_)).getOrElse(BigDecimal(0)).compareTo(BigDecimal(0))
    (
      "@months_ago" #> SHtml.select(listOfTags, Box!! monthsAgoVar.is, monthsAgoVar(_)) &
      "#account-balance span" #> s"$amount $currency" &
      "#account-balance [class]" #> {if(compared == 1) "green--color" else if(compared == -1) "orange--color" else ""} &

      "#credit-score-star-1 [class]" #> generateStars(creditRatingToInt, 1) &
      "#credit-score-star-2 [class]" #> generateStars(creditRatingToInt, 2) &
      "#credit-score-star-3 [class]" #> generateStars(creditRatingToInt, 3) &
      "#credit-score-star-4 [class]" #> generateStars(creditRatingToInt, 4) &
      "#credit-score-star-5 [class]" #> generateStars(creditRatingToInt, 5) &

      "#profile-completeness-star-1 [class]" #> generateStars(profileCompletenessToInt, 1) &
      "#profile-completeness-star-2 [class]" #> generateStars(profileCompletenessToInt, 2) &
      "#profile-completeness-star-3 [class]" #> generateStars(profileCompletenessToInt, 3) &
      "#profile-completeness-star-4 [class]" #> generateStars(profileCompletenessToInt, 4) &
      "#profile-completeness-star-5 [class]" #> generateStars(profileCompletenessToInt, 5) &
        
      // Replace the type=submit with Javascript that makes the ajax call.
      "type=submit" #> SHtml.ajaxSubmit(S.?("button.show"), process)
     ).apply(xhtml)
  }

  def generateStars(rating: Int, nthStar: Int): String = {
    if(rating>=nthStar) "fa fa-star color-of-star" else "fa fa-star-o color-of-star"
  }

  def calculateTransactionOverview(bankId: String, accountId: String, isPositiveAmount: Boolean, monthsAgo: Int): Box[Map[String, Double]] = {

    val transactionsForAccount : Box[TransactionsJson] = 
      ObpAPI.transactions(bankId, accountId, CUSTOM_OWNER_VIEW_ID, Some(10000), None, Some(Util.monthsAgo(monthsAgo)), Some(now), Some(DESC))

    val transactionsOverview = transactionsForAccount map { x =>
      val transactionsJson: List[TransactionJson] = x.transactions.getOrElse(Nil)
      val tempList: List[(List[TransactionTagJson], Option[TransactionValueJson])] =
        transactionsJson.map(x => (x.tagJsons.getOrElse(Nil), x.details.flatMap(_.value)))
      val transactions = isPositiveAmount match {
        case true => tempList.filter(i => i._2.map(_.amount).flatten.getOrElse("0").toDouble > 0)
        case false => tempList.filter(i => i._2.map(_.amount).flatten.getOrElse("0").toDouble < 0)
      }
      val categorisedTransactions: List[(String, Option[String], Option[String])] =
        transactions.map( x =>
          (x._1.flatMap(_.value).headOption.getOrElse("Unknown"),
            x._2.flatMap(_.amount),
            x._2.flatMap(_.currency)
          )
        )
      val groupedTransactionsByCategoryAndCurrency: Map[String, List[(String, Option[String], Option[String])]] = categorisedTransactions.groupBy(i => i._1)
      val sumByCategory: Map[String, Double] = groupedTransactionsByCategoryAndCurrency.map(i => (i._1, i._2.map(_._2.getOrElse("0").toDouble).sum))
      sumByCategory
    }
    logger.debug(transactionsOverview)
    transactionsOverview
  }
  
}
