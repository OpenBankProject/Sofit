package code.snippet

import code.Constant._
import code.lib.ObpAPI.{createIncome, getAccount}
import code.util.Helper.{MdcLoggable, getAccountTitle}
import net.liftweb.http.{S, SHtml}
import net.liftweb.http.js.JE.Call
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds.SetHtml
import net.liftweb.util.Helpers._

import scala.xml.{NodeSeq, Text}


class CreateIncome(params: List[String]) extends MdcLoggable {
  val bankId = params(0)
  val accountId = params(1)
  val accountJson = getAccount(bankId, accountId, CUSTOM_OWNER_VIEW_ID).openOrThrowException("Could not open accountJson")
  def accountTitle = ".account-title *" #> getAccountTitle(accountJson)

  //set up ajax handlers to edit account label
  def addIncome(xhtml: NodeSeq): NodeSeq = {
    var incomeDescription = ""
    var incomeAmount = "0"

    def process(): JsCmd = {
      logger.debug(s"CreateIncome.addIncome.process: edit label $incomeDescription")
      val result = createIncome(bankId, accountId, incomeDescription, incomeAmount, "EUR")
      if (result.isDefined) {
        val msg = "Saved"
        Call("socialFinanceNotifications.notify", msg).cmd
        S.redirectTo(s"/banks/$bankId/accounts/$accountId/owner")
      } else {
         val msg = "Sorry, Income" + incomeDescription + " could not be added ("+ result +")"
         Call("socialFinanceNotifications.notifyError", msg).cmd
      }
    }

    (
      "@income_description" #> SHtml.text("", s => incomeDescription = s) &
      "@income_amount" #> SHtml.text("", s => incomeAmount = s) &
        // Replace the type=submit with Javascript that makes the ajax call.
        "type=submit" #> SHtml.ajaxSubmit(S.?("button.save"), process)
      ).apply(xhtml)
  }
}
