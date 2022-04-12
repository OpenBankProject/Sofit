package code.snippet

import code.Constant._
import code.lib.ObpAPI
import code.lib.ObpAPI.{createOutcome, getAccount}
import code.util.Helper.{MdcLoggable, getAccountTitle}
import net.liftweb.common.Box
import net.liftweb.http.js.JE.Call
import net.liftweb.http.js.JsCmd
import net.liftweb.http.{RequestVar, S, SHtml}
import net.liftweb.util.Helpers._

import scala.xml.NodeSeq


class CreateExpenditure(params: List[String]) extends MdcLoggable {
  private object tagVar extends RequestVar("")
  val bankId = params(0)
  val accountId = params(1)
  val accountJson = getAccount(bankId, accountId, CUSTOM_OWNER_VIEW_ID).openOrThrowException("Could not open accountJson")
  def accountTitle = ".account-title *" #> getAccountTitle(accountJson)
  
  
  def addPayment(xhtml: NodeSeq): NodeSeq = {
    var outcomeDescription = ""
    var outcomeAmount = 0D
    val listOfTags: Seq[(String, String)] = S.?("expenditure.tags").
      split(",").toList.map(_.trim).map(i => i.replaceAll("_", "/")).map(i => (i,i))

    def process(): JsCmd = {
      logger.debug(s"CreateOutcome.addOutcome.process: edit label $outcomeDescription")
      val result = createOutcome(bankId, accountId, outcomeDescription, outcomeAmount.toString, "EUR")
      if (result.isDefined) {
        val transactionId = result.map(_.transaction_id).getOrElse("")
        ObpAPI.addTags(bankId, accountId, "owner", transactionId, List(tagVar.is))
        val msg = "Saved"
        Call("socialFinanceNotifications.notify", msg).cmd
        S.redirectTo(s"/banks/$bankId/accounts/$accountId/owner")
      } else {
         val msg = s"Sorry, Expenditure $outcomeDescription could not be added ($result)"
         Call("socialFinanceNotifications.notifyError", msg).cmd
      }
    }

    (
      "@payment_description" #> SHtml.text("", s => outcomeDescription = s) &
       "#payment_category" #> SHtml.select(listOfTags, Box!! tagVar.is, tagVar(_)) &
      "@payment_amount" #> SHtml.number(0D, (s:Double) => outcomeAmount = s, 0D, 1000000000D, 0.01D) &
        // Replace the type=submit with Javascript that makes the ajax call.
        "type=submit" #> SHtml.ajaxSubmit(S.?("button.save"), process)
      ).apply(xhtml)
  }
}
