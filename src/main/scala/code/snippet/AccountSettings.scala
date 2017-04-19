package code.snippet

import scala.xml.{NodeSeq, Text}

import code.util.Helper.MdcLoggable
import net.liftweb.http.js.JE.Call
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds.SetHtml
import net.liftweb.http.{S, SHtml}
import net.liftweb.json._
import net.liftweb.util.Helpers._

import code.lib.ObpAPI.{updateAccountLabel, getAccount}
import code.util.Helper.getAccountTitle


/*
For maintaining permissions on the views (entitlements on the account)
 */
class AccountSettings(params: List[String]) extends MdcLoggable {
  val bankId = params(0)
  val accountId = params(1)
  val accountJson = getAccount(bankId, accountId, "owner").openOrThrowException("Could not open accountJson")
  def accountTitle = ".account-title *" #> getAccountTitle(accountJson)

  //set up ajax handlers to edit account label
  def editLabel(xhtml: NodeSeq): NodeSeq = {
    var newLabel = ""

    def process(): JsCmd = {
      logger.debug(s"AccountSettings.editLabel.process: edit label $newLabel")
      val result = updateAccountLabel(bankId, accountId, newLabel)
      if (result.isDefined) {
        val msg = "Label " + newLabel + " has been set"
        SetHtml("account-title", Text(newLabel)) &
        Call("socialFinanceNotifications.notify", msg).cmd
      } else {
         val msg = "Sorry, Label" + newLabel + " could not be set ("+ result +")"
         Call("socialFinanceNotifications.notifyError", msg).cmd
      }
    }

    (
      // Bind newViewName field to variable (e.g. http://chimera.labs.oreilly.com/books/1234000000030/ch03.html)
      "@new_label" #> SHtml.text(accountJson.label.getOrElse(""), s => newLabel = s) &
        // Replace the type=submit with Javascript that makes the ajax call.
        "type=submit" #> SHtml.ajaxSubmit("Edit label", process)
      ).apply(xhtml)
  }
}
