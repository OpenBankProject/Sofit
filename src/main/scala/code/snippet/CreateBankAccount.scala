package code.snippet

import code.lib.ObpAPI
import code.lib.ObpAPI.createAccount
import code.lib.ObpJson.BankJson400
import code.util.Helper.MdcLoggable
import net.liftweb.common.Box
import net.liftweb.http.js.JE.Call
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds.SetHtml
import net.liftweb.http.{RequestVar, SHtml}
import net.liftweb.util.Helpers._
import net.liftweb.util.Props

import scala.collection.immutable.List
import scala.xml.{NodeSeq, Text}


class CreateBankAccount(params: List[BankJson400]) extends MdcLoggable {

  private object bankVar extends RequestVar("")
  private object userIdVar extends RequestVar("")
  
  
  def editLabel(xhtml: NodeSeq): NodeSeq = {
    var newLabel = ""
    val bankId = "user." + ObpAPI.currentUser.map(_.user_id).getOrElse(System.currentTimeMillis())
    val listOfBanks = params
      .filter(_.id == bankId)
      .map(b => (b.id, b.full_name))

    def process(): JsCmd = {
      ObpAPI.currentUser.map {
        u => userIdVar.set(u.user_id)
      }
      logger.debug(s"CreateBankAccount.editLabel.process: edit label $newLabel")
      if(listOfBanks.size == 1) {
        val result = createAccount(bankVar.is, newLabel, userIdVar.is)
        if (result.isDefined) {
          val msg = "Saved"
          SetHtml("account-title", Text(newLabel)) &
            Call("socialFinanceNotifications.notify", msg).cmd
        } else {
          val msg = "Sorry, the new account with the label" + newLabel + " could not be set ("+ result +")"
          Call("socialFinanceNotifications.notifyError", msg).cmd
        }
      } else {
        val msg = "Sorry, the new account with the label" + newLabel + " could not be set due to unresolved bank id value"
        Call("socialFinanceNotifications.notifyError", msg).cmd
      }
    }

    (
      "@new_label" #> SHtml.text("", s => newLabel = s) &
       "#bank-id" #> SHtml.select(listOfBanks, Box!! bankVar.is, bankVar(_)) &
       // Replace the type=submit with Javascript that makes the ajax call.
       "type=submit" #> SHtml.ajaxSubmit("Create", process)
      ).apply(xhtml)
  }
}
