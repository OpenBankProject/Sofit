package code.snippet

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
import net.liftweb.common.{Loggable, Box}
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
  val bank = viewsDataJson.bankId
  val account = viewsDataJson.accountId

  def getTableContent(xhtml: NodeSeq) :NodeSeq = {

    //add ajax callback to save view
    def saveOnClick(viewId : String): CssSel = {
      implicit val formats = DefaultFormats

      ".save-button [data-id]" #> viewId &
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
      ".delete-button [data-id]" #> viewId &
        ".delete-button [onclick+]" #> SHtml.ajaxCall(Call("collectData", Str(viewId)), callResult => {
          val result = ObpAPI.deleteView(viewsDataJson.bankId, viewsDataJson.accountId, viewId)

          if(result) {
            val msg = "View " + viewId + " has been deleted"
            Call("socialFinanceNotifications.notify", msg).cmd
            RedirectTo("")
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
    val addEditSel      = ".edit"        #> ids.map(x => "* [data-id]" #> x)
    val addSaveSel      = ".save"        #> ids.map(x => ("* [data-id]" #> x) & deleteOnClick(x) & saveOnClick(x))
    val addCancelSel    = ".cancel"      #> ids.map(x => "* [data-id]" #> x)


    val permissionNames = permissions.keys
    val permSel = ".permissions *" #>
      permissionNames.map(
        permName => {
          ".permission_name *"  #> permName.capitalize.replace("_", " ") &
          ".permission_value *" #> getPermissionValues(permName)
        }
      )
      (viewNameSel &
        shortNamesSel &
        aliasSel &
        descriptionSel &
        isPublicSel &
        permSel &
        addEditSel &
        addSaveSel &
        addCancelSel
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

        val checkBox =
          checked &
            ".is_public_cb [data-viewid]" #> viewId

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

        val checkBox =
          checked &
          ".permission_value_cb [value]" #> permName &
          ".permission_value_cb [name]" #> permName &
          ".permission_value_cb [data-viewid]" #> viewId

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
        val result = addView(bank, account, newViewName)

        if (result.isDefined) {
          val msg = "View " + newViewName + " has been created"
          Call("socialFinanceNotifications.notify", msg).cmd
          //reload page for new view to be shown
          RedirectTo("")
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
      "type=submit" #> ajaxSubmit("OK", process)
    ).apply(xhtml)
  }
}
