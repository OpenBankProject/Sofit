package code.snippet

import net.liftweb.common.Loggable
import net.liftweb.http.S
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds._
import scala.xml.Text
import net.liftweb.util.Helpers._
import code.lib.ObpAPI
import net.liftweb.http.SHtml
import net.liftweb.common.Failure
import code.lib.ObpJson.PermissionsJson
import code.lib.ObpJson.AccountJson
import code.lib.ObpJson.ViewJson
import net.liftweb.util.CssSel
import code.util.Helper.getAccountTitle


class CreatePermissionForm(params : (List[ViewJson], AccountJson, PermissionsUrlParams)) extends Loggable {
    case class ViewData(view : ViewJson, allowed: Boolean)
    
    val views = params._1
    val accountJson = params._2
    val urlParams = params._3
    
    val url = S.uri.split("/")
    var email = ""
        
    val allowed : scala.collection.mutable.Map[ViewJson, Boolean] = scala.collection.mutable.Map(views.map(v => (v, false)).toSeq : _*)
      
    val nonPublicViews = views.filterNot(_.is_public.getOrElse(true))
    val nonBadViews = nonPublicViews.filterNot(_.id == None)
    def viewData = nonBadViews.map(view => ViewData(view, allowed(view)))
    
    def render = {

      def process(): JsCmd = {

        def showMsg(msg : String) = {
          SetHtml("create-permission-message", Text(msg))
        }
        
        def invalidEmail() : Boolean = !email.contains("@")
        
        if(email.isEmpty()) showMsg("You must enter an e-mail address")
        else if(invalidEmail()) showMsg("Invalid e-mail address")
        //else if(viewData.forall(vData => allowed(vData.view) == false)) showMsg("You must select at least one view to grant access to.")
        else if(viewData.forall(_.allowed == false)) showMsg("You must select at least one view to grant access to.")
        else {
          //create the permission and return any failure
          if(url.length > 4) {
            val bankId = url(2)
            val accountId = url(4)
            val viewIds = for {
              vData <- viewData
              if(vData.allowed)
              vId <- vData.view.id
            } yield vId
            
            val result = ObpAPI.addPermissions(bankId, accountId, email, viewIds)
            result match {
              case Failure(msg, _, _) => showMsg(msg)
              case _ => {
                //Redirect to permissions overview
                //TODO: Would be nice to calculate this but at the moment there is nothing to do it
                S.redirectTo("/banks/" + bankId + "/accounts/" + accountId + "/permissions")
                Noop
              }
            }
            
          } else {
            logger.warn("Couldn't determine bank and account from url:" + url.toList.toString)
            showMsg("error")
          }
          
        }
        
      }
      
      ".view_row *" #> viewData.map(vData => {
        val onOffSwitch = "onoffswitch-view-" + vData.view.id.getOrElse("")
        ".view_name *" #> vData.view.short_name.getOrElse(vData.view.id.getOrElse("")) &
        ".view_check" #> SHtml.checkbox(
          allowed(vData.view),
          allowed.put(vData.view, _),
          "class" -> "onoffswitch-checkbox view_check",
          "id" -> onOffSwitch) &
        ".onoffswitch-label [for]" #> onOffSwitch
      }) &
      "name=email" #> SHtml.text(email, email = _) &
      "type=submit" #> SHtml.ajaxSubmit("Grant access", process)
      
    }

    def accountTitle = ".account-title *" #> getAccountTitle(accountJson)
  }
