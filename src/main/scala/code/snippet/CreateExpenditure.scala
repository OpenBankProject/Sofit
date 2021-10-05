package code.snippet

import code.Constant._
import code.lib.ObpAPI.{createOutcome, getAccount}
import code.util.Helper.{MdcLoggable, getAccountTitle}
import net.liftweb.http.SHtml
import net.liftweb.http.js.JE.Call
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds.SetHtml
import net.liftweb.util.Helpers._

import scala.xml.{NodeSeq, Text}


class CreateExpenditure(params: List[String]) extends MdcLoggable {
  val bankId = params(0)
  val accountId = params(1)
  val accountJson = getAccount(bankId, accountId, CUSTOM_OWNER_VIEW_ID).openOrThrowException("Could not open accountJson")
  def accountTitle = ".account-title *" #> getAccountTitle(accountJson)

  //set up ajax handlers to edit account label
  def addPayment(xhtml: NodeSeq): NodeSeq = {
    var outcomeDescription = ""
    var outcomeAmount = "0"

    def process(): JsCmd = {
      logger.debug(s"CreateOutcome.addOutcome.process: edit label $outcomeDescription")
      val result = createOutcome(bankId, accountId, outcomeDescription, outcomeAmount, "EUR")
      if (result.isDefined) {
        val msg = "Income " + outcomeDescription + " has been set"
        SetHtml("account-title", Text(outcomeDescription)) &
        Call("socialFinanceNotifications.notify", msg).cmd
      } else {
         val msg = "Sorry, Income" + outcomeDescription + " could not be added ("+ result +")"
         Call("socialFinanceNotifications.notifyError", msg).cmd
      }
    }

    (
      "@payment_description" #> SHtml.text("", s => outcomeDescription = s) &
      "@payment_amount" #> SHtml.text("", s => outcomeAmount = s) &
        // Replace the type=submit with Javascript that makes the ajax call.
        "type=submit" #> SHtml.ajaxSubmit("Add payment", process)
      ).apply(xhtml)
  }
}
