package code.snippet

import code.Constant._
import code.lib.ObpAPI.{createOutcome, getAccount}
import code.util.Helper.{MdcLoggable, getAccountTitle}
import net.liftweb.common.Box
import net.liftweb.http.{RequestVar, S, SHtml}
import net.liftweb.http.js.JE.Call
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds.SetHtml
import net.liftweb.util.Helpers._
import net.liftweb.util.Props

import scala.xml.{NodeSeq, Text}


class CreateExpenditure(params: List[String]) extends MdcLoggable {
  private object tagVar extends RequestVar("")
  val bankId = params(0)
  val accountId = params(1)
  val accountJson = getAccount(bankId, accountId, CUSTOM_OWNER_VIEW_ID).openOrThrowException("Could not open accountJson")
  def accountTitle = ".account-title *" #> getAccountTitle(accountJson)
  
  
  def addPayment(xhtml: NodeSeq): NodeSeq = {
    var outcomeDescription = ""
    var outcomeAmount = "0"
    val listOfTags: Seq[(String, String)] = Props.get("expenditure.tags", "None,Food,Rent,Transport,Health_Insurance,Other").
      split(",").toList.map(_.trim).map(i => i.replaceAll("_", "/")).map(i => (i,i))

    def process(): JsCmd = {
      logger.debug(s"CreateOutcome.addOutcome.process: edit label $outcomeDescription")
      val result = createOutcome(bankId, accountId, outcomeDescription, outcomeAmount, "EUR")
      if (result.isDefined) {
        val msg = "Saved"
        Call("socialFinanceNotifications.notify", msg).cmd
        S.redirectTo(s"/banks/$bankId/accounts/$accountId/owner")
      } else {
         val msg = "Sorry, Income" + outcomeDescription + " could not be added ("+ result +")"
         Call("socialFinanceNotifications.notifyError", msg).cmd
      }
    }

    (
      "@payment_description" #> SHtml.text("", s => outcomeDescription = s) &
       "#payment_category" #> SHtml.select(listOfTags, Box!! tagVar.is, tagVar(_)) &
      "@payment_amount" #> SHtml.text("", s => outcomeAmount = s) &
        // Replace the type=submit with Javascript that makes the ajax call.
        "type=submit" #> SHtml.ajaxSubmit(S.?("button.save"), process)
      ).apply(xhtml)
  }
}
