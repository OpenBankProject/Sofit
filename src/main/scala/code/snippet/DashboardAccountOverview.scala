package code.snippet

import _root_.net.liftweb.http.js.{JE, JsCmd, JsCmds}
import code.Constant._
import code.lib.ObpAPI
import code.lib.ObpAPI.{DESC, getAccount}
import code.lib.ObpJson.{TransactionJson, TransactionTagJson, TransactionValueJson, TransactionsJson}
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
  val accountJson = getAccount(bankId, accountId, CUSTOM_OWNER_VIEW_ID).openOrThrowException("Could not open accountJson")
  def accountTitle = ".account-title *" #> getAccountTitle(accountJson)

  //set up ajax handlers to edit account label
  def calculateExpenditureOverview(xhtml: NodeSeq): NodeSeq = {
    
    def process(): JsCmd = {
      val expenditures: List[(String, Double)] = calculateTransactionOverview(bankId, accountId, false,monthsAgoVar.is.toInt).map(_.toList)
        .toList.flatten.sortBy(_._2)
      val expenditureData = expenditures.map(i => s"['${i._1}',${i._2.abs}]").mkString(",")
      val incomes: List[(String, Double)] = calculateTransactionOverview(bankId, accountId, true,monthsAgoVar.is.toInt).map(_.toList)
        .toList.flatten.sortBy(_._2)
      val incomeMaxValue = incomes.map(_._2).max.abs
      val incomeData = incomes.map(i => s"['${i._1}',${i._2.abs}]").mkString(",")
      
      val script = s"""google.charts.load('current', {'packages':['corechart']});
                     |google.charts.setOnLoadCallback(drawChart);
                     |
                     |function drawChart() {
                     |var expenditureData = google.visualization.arrayToDataTable([
                     |  ['Category', '€'],
                     |  $expenditureData
                     |]);
                     |
                     |var expenditureOptions = {
                     |  title:'Expenditure'
                     |};
                     |
                     |var expenditureChart = new google.visualization.BarChart(document.getElementById('expenditureChart'));
                     |  expenditureChart.draw(expenditureData, expenditureOptions);
                     |  
                     |var incomeData = google.visualization.arrayToDataTable([
                     |  ['Category', '€'],
                     |  $incomeData
                     |]);
                     |
                     |var incomeOptions = {
                     |  title:'Income'
                     |};
                     |
                     |var incomeChart = new google.visualization.BarChart(document.getElementById('incomeChart'));
                     |  incomeChart.draw(incomeData, incomeOptions);
                     |  
                     |}""".stripMargin
      
      JsRaw(script)
    }
    
    (
      "@months_ago" #> SHtml.select(listOfTags, Box!! monthsAgoVar.is, monthsAgoVar(_)) &
        // Replace the type=submit with Javascript that makes the ajax call.
        "type=submit" #> SHtml.ajaxSubmit(S.?("button.show"), process)
        
      ).apply(xhtml)
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
