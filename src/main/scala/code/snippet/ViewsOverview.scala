package code.snippet

import net.liftweb.util.Helpers._
import scala.xml.NodeSeq
import code.lib.ObpJson.CompleteViewJson
import net.liftweb.util.CssSel
import net.liftweb.http.SHtml
import net.liftweb.json.JsonAST.JValue
import net.liftweb.json._
import net.liftweb.http.js.JsCmds.Alert
import net.liftweb.common.Box
import code.lib.ObpAPI
import net.liftweb.http.S

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
class ViewsOverview(viewsDataJson: ViewsDataJSON) {
  val views = viewsDataJson.views

  def getTableContent(xhtml: NodeSeq) :NodeSeq = {

    def saveOnClick(viewId : String): CssSel = {
      import net.liftweb.http.js.JE.{Call,Str}
      implicit val formats = DefaultFormats

      ".save-button [data-id]" #> viewId &
      ".save-button [onclick+]" #> SHtml.ajaxCall(Call("collectData", Str(viewId)), callResult => {
        val result: Box[Unit] = for{
          data <- tryo{parse(callResult).extract[ViewUpdateData]}
          response <- ObpAPI.updateView(viewsDataJson.bankId, viewsDataJson.accountId, viewId, data.updateJson)
        } yield{
          response
        }
        if(result.isDefined){
          val msg = "View " + viewId + " has been updated"
          Call("socialFinanceNotificiations.notify", msg).cmd
        }
        else{
          val msg = "Error updating view"
          Call("socialFinanceNotificiations.notifyError", msg).cmd
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
    val viewNameSel     = ".view_name *"      #> views.map( view => view.shortName.getOrElse(""))
    val shortNamesSel   = ".short_name"       #> views.map( view => "* *" #> view.shortName.getOrElse("") & "* [data-viewid]" #> view.id )
    val aliasSel        = ".alias"            #> views.map( view => "* *" #> aliasType(view.alias) & "* [data-viewid]" #> view.id )
    val descriptionSel  = ".description"      #> views.map( view => ".desc *" #> view.description.getOrElse("") & "* [data-viewid]" #> view.id )
    val isPublicSel     = ".is_public *"      #> getIfIsPublic()
    val addDeleteSel    = ".delete"           #> ids.map(x => "* [data-id]" #> x)
    val addEditSel      = ".edit"             #> ids.map(x => "* [data-id]" #> x)
    val addSaveSel      = ".save"             #> ids.map(x => ("* [data-id]" #> x) & saveOnClick(x))
    val addCancelSel    = ".cancel"           #> ids.map(x => "* [data-id]" #> x)


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
        addDeleteSel &
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
  }