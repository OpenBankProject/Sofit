package code.snippet

import scala.collection.immutable.{HashSet, TreeMap}
import scala.collection._
import code.util.Helper._
import net.liftweb.http.js.JE.{Call, Str}
import net.liftweb.http.js.JsCmd
import net.liftweb.util.Helpers._
import scala.xml.{Node, NodeSeq, Text}
import code.lib.ObpJson.CompleteViewJson
import net.liftweb.util.CssSel
import net.liftweb.http.{S, SHtml}
import net.liftweb.json.JsonAST.JValue
import net.liftweb.json._
import net.liftweb.http.js.JsCmds.{SetHtml, Alert, RedirectTo}
import net.liftweb.common.{Full, Loggable, Box}
import code.lib.ObpAPI
import net.liftweb.http.SHtml.{text,ajaxSubmit, ajaxButton}
import ObpAPI.{addView, deleteView}
import SHtml._

case class ViewUpdateData(
  viewId: String,
  updateJson: JValue
)

case class ViewsDataJSON(
   views: List[CompleteViewJson],
   bankId: String,
   accountId: String
)

/*
For maintaining permissions on the views (entitlements on the account)
 */
class ViewsOverview(viewsDataJson: ViewsDataJSON) extends Loggable {
  val views = viewsDataJson.views
  val bankId = viewsDataJson.bankId
  val accountId = viewsDataJson.accountId

  def accountTitle = ".account-title *" #> getAccountTitle(bankId, accountId)

  def getTableContent(xhtml: NodeSeq) :NodeSeq = {

    //add ajax callback to save view
    def saveOnClick(viewId : String): CssSel = {
      implicit val formats = DefaultFormats

      ".save-button [onclick+]" #> SHtml.ajaxCall(Call("collectData", Str(viewId)), callResult => {
        val result: Box[Unit] = for {
          data <- tryo{parse(callResult).extract[ViewUpdateData]}
          response <- ObpAPI.updateView(viewsDataJson.bankId, viewsDataJson.accountId, viewId, data.updateJson)
        } yield{
          response
        }
        if(result.isDefined) {
          val msg = "View " + viewId + " has been updated"
          Call("socialFinanceNotifications.notify", msg).cmd
        }
        else {
          val msg = "Error updating view"
          Call("socialFinanceNotifications.notifyError", msg).cmd
        }
      })
    }

    def deleteOnClick(viewId : String): CssSel = {
      ".delete-button [onclick+]" #> SHtml.ajaxCall(Call("collectData", Str(viewId)), callResult => {
        val result = ObpAPI.deleteView(viewsDataJson.bankId, viewsDataJson.accountId, viewId)

        if(result) {
          val msg = "View " + viewId + " has been deleted"
          Call("socialFinanceNotifications.notifyReload", msg).cmd
        }
        else {
          val msg = "Error deleting view"
          Call("socialFinanceNotifications.notifyError", msg).cmd
        }
      })
    }

    val permissionsCollection: List[Map[String, Boolean]] = views.map(view => view.permissions)


    // Use permissions from the first view as a basis for permission names.
    val permissions: Map[String, Boolean] = permissionsCollection(0)

    def aliasType(typeInJson : Option[String]) = {
      typeInJson match {
        case Some("") => "none (display real names only)"
        case Some(s) => s
        case _ => ""
      }
    }
    
    val ids = getIds()
    val viewNameSel     = ".view_name *" #> views.map( view => view.shortName.getOrElse(""))
    val shortNamesSel   = ".short_name"  #> views.map( view => "* *" #> view.shortName.getOrElse("") & "* [data-viewid]" #> view.id )
    val aliasSel        = ".alias"       #> views.map( view => "* *" #> aliasType(view.alias) & "* [data-viewid]" #> view.id )
    val descriptionSel  = ".description" #> views.map( view => ".desc *" #> view.description.getOrElse("") & "* [data-viewid]" #> view.id )
    val isPublicSel     = ".is_public *" #> getIfIsPublic()
    var actionsSel      = ".action-buttons" #> ids.map(x => ("* [data-id]" #> x) & saveOnClick(x) & deleteOnClick(x))
    // permissions.keys need to be converted to sequence to retain the same order when running map over it!
    // Also sorting it
    val permissionNames = permissions.keys.toSeq.sortWith(_ < _)
    val permSel = ".permissions *" #> {
      val permissionsCssSel = permissionNames.map(
        permName => {
          val foo = permName
          ".permission_name *"  #> permName.capitalize.replace("_", " ") &
          ".permission_value *" #> getPermissionValues(permName)
        }
      )
      permissionsCssSel
    }
      (viewNameSel &
        shortNamesSel &
        aliasSel &
        descriptionSel &
        isPublicSel &
        permSel &
        actionsSel
      ).apply(xhtml)
  }

  def getIds(): List[String] = {
    views.map( view => view.id.getOrElse(""))
  }


  def getIfIsPublic() :List[CssSel] = {
    views.map(
      view => {
        val isPublic = view.isPublic.getOrElse(false)
        val viewId: String = view.id.getOrElse("")
        val checked =
          if(isPublic)
            ".is_public_cb [checked]" #> "checked" &
            ".is_public_cb [disabled]" #> "disabled"
        else
            ".is_public_cb [disabled]" #> "disabled"

        val id = "myonoffswitch-public-" + viewId
        val checkBox =
          checked &
            ".is_public_cb [data-viewid]" #> viewId &
            ".onoffswitch-label [for]" #> id &
            ".is_public_cb [id]" #> id

        checkBox
      }
    )
  }

  def getPermissionValues(permName: String) :List[CssSel] = {
    views.map(
      view => {
        val permValue: Boolean = view.permissions(permName)
        val viewId: String = view.id.getOrElse("")
        val checked =
          if(permValue){
            ".permission_value_cb [checked]" #> "checked" &
            ".permission_value_cb [disabled]" #> "disabled"
          }
          else
            ".permission_value_cb [disabled]" #> "disabled"

        val id = "myonoffswitch-permission-" + permName + "-" + viewId
        val checkBox =
          checked &
          ".permission_value_cb [value]" #> permName &
          ".permission_value_cb [name]" #> permName &
          ".permission_value_cb [data-viewid]" #> viewId &
          ".onoffswitch-label [for]" #> id &
          ".permission_value_cb [id]" #> id

        checkBox
      }
    )
  }

  //set up ajax handlers to add a new view
  def setupAddView(xhtml: NodeSeq): NodeSeq = {
    var newViewName = ""

    def process(): JsCmd = {
      logger.debug(s"ViewsOverview.setupAddView.process: create view called $newViewName")

      if (views.find { case (v) => v.shortName.get == newViewName }.isDefined) {
        val msg = "Sorry, a View with that name already exists."
        Call("socialFinanceNotifications.notifyError", msg).cmd
      } else {
        // This only adds the view (does not grant the current user access)
        val result = addView(bankId, accountId, newViewName)

        if (result.isDefined) {
          val msg = "View " + newViewName + " has been created. Don't forget to grant yourself or others access."
          Call("socialFinanceNotifications.notifyReload", msg).cmd
          // After creation, current user does not have access so, we show message above.
        } else {
          val msg = "View " + newViewName + " could not be created"
          Call("socialFinanceNotifications.notifyError", msg).cmd
        }
      }
    }

    (
      // Bind newViewName field to variable (e.g. http://chimera.labs.oreilly.com/books/1234000000030/ch03.html)
      "@new_view_name" #> text(newViewName, s => newViewName = s) &
      // Replace the type=submit with Javascript that makes the ajax call.
      "type=submit" #> ajaxSubmit("Save", process)
    ).apply(xhtml)
  }
}
