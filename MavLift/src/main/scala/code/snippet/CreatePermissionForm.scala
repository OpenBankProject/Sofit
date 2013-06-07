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

object CreatePermissionForm extends Loggable {
    case class PermissionData(viewName : String, allowed: Boolean)
    
    def render = {
      val url = S.uri.split("/")
      var email = ""
      var owner = false
      var management = false
      var ournetwork = false
      var team = false
      var board = false
      var authorities = false
      
      def permissions = List(PermissionData("owner", owner), PermissionData("management", management), PermissionData("ournetwork", ournetwork),
          PermissionData("team", team), PermissionData("board", board), PermissionData("authorities", authorities))
        
      def process(): JsCmd = {
        
        def showMsg(msg : String) = {
          SetHtml("create-permission-message", Text(msg))
        }
        
        def invalidEmail() : Boolean = !email.contains("@")
        
        if(permissions.forall(_.allowed == false)) showMsg("You must select at least one view to grant access to.")
        else if(email.isEmpty()) showMsg("You must enter an email address")
        else if(invalidEmail()) showMsg("Invalid email address")
        else {
          //create the permission and return any failure
          if(url.length > 5) {
            val bankId = url(3)
            val accountId = url(5)
            val viewNames = for {
              permission <- permissions
              if(permission.allowed)
            } yield permission.viewName
            
            val result = ObpAPI.addPermissions(bankId, accountId, email, viewNames)
            result match {
              case Failure(msg, _, _) => showMsg(msg)
              case _ => {
                //Redirect to permissions overview
                S.redirectTo("/permissions/banks/" + bankId + "/accounts/" + accountId) //TODO: Would be nice to calculate this but at the moment there is nothing to do it
                Noop
              }
            }
            
          } else {
            logger.warn("Couldn't determine bank and account from url:" + url.toList.toString)
            showMsg("error")
          }
          
        }
        
      }
      
      "name=owner" #> SHtml.checkbox(owner, owner = _) &
      "name=management" #> SHtml.checkbox(management, management = _) &
      "name=ourNetwork" #> SHtml.checkbox(ournetwork, ournetwork = _) &
      "name=team" #> SHtml.checkbox(team, team = _) &
      "name=board" #> SHtml.checkbox(board, board = _) &
      "name=authorities" #> SHtml.checkbox(authorities, authorities = _) &
      "name=email" #> (SHtml.text(email, email = _) ++ SHtml.hidden(process))
    }
  }