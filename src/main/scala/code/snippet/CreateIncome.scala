package code.snippet

import code.Constant._
import code.lib.ObpAPI
import code.lib.ObpAPI.{createIncome, getAccount, getDoubleEntryTransaction}
import code.util.Helper.{MdcLoggable, getAccountTitle}
import net.liftweb.common.Box
import net.liftweb.http.js.JE.Call
import net.liftweb.http.js.JsCmd
import net.liftweb.http.{RequestVar, S, SHtml}
import net.liftweb.util.Helpers._
import net.liftweb.util.Props

import scala.xml.NodeSeq


class CreateIncome(params: List[String]) extends MdcLoggable {
  private object tagVar extends RequestVar("")
  val bankId = params(0)
  val accountId = params(1)
  val accountJson = getAccount(bankId, accountId, CUSTOM_OWNER_VIEW_ID).openOrThrowException("Could not open accountJson")
  def accountTitle = ".account-title *" #> getAccountTitle(accountJson)

  //set up ajax handlers to edit account label
  def addIncome(xhtml: NodeSeq): NodeSeq = {
    var incomeDescription = ""
    var incomeAmount = 0D
    val listOfTags: Seq[(String, String)] = S.?("income.tags").
      split(",").toList.map(_.trim).map(i => i.replaceAll("_", "/")).map(i => (i,i))

    def process(): JsCmd = {
      logger.debug(s"CreateIncome.addIncome.process: edit label $incomeDescription")
      val result = createIncome(bankId, accountId, incomeDescription, incomeAmount.toString, "EUR")
      if (result.isDefined) {
        val incomeAccountId = Props.get("incoming.account_id", "")
        val transactionId = result.map(_.transaction_id).getOrElse("")
        val creditTransactionId = getDoubleEntryTransaction(bankId, incomeAccountId, "owner", transactionId)
          .map(_.credit_transaction.transaction_id).getOrElse("")
        ObpAPI.addTags(bankId, accountId, "owner", creditTransactionId, List(tagVar.is))
        val msg = "Saved"
        Call("socialFinanceNotifications.notify", msg).cmd
        S.redirectTo(s"/banks/$bankId/accounts/$accountId/owner")
      } else {
         val msg = s"Sorry, Income $incomeDescription could not be added ($result)"
         Call("socialFinanceNotifications.notifyError", msg).cmd
      }
    }

    (
      "@income_description" #> SHtml.text("", s => incomeDescription = s) & 
        "#income_category" #> SHtml.select(listOfTags, Box!! tagVar.is, tagVar(_)) &
      "@income_amount" #> SHtml.number(0D, (s:Double) => incomeAmount = s, 0D, 1000000000D, 0.01D) &
        // Replace the type=submit with Javascript that makes the ajax call.
        "type=submit" #> SHtml.ajaxSubmit(S.?("button.save"), process)
      ).apply(xhtml)
  }
}
