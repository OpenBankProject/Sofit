package code.snippet

import net.liftweb.util.Helpers._
import scala.xml.NodeSeq

import code.lib.ObpJson.CompleteViewJson
import net.liftweb.util.CssSel


class ViewsOverview(views : List[CompleteViewJson]) {

  def getTableContent(xhtml: NodeSeq) :NodeSeq = {

    val permissionsCollection: List[Map[String, Boolean]] = views.map(view => view.permissions)
    val permissions: Map[String, Boolean] = permissionsCollection(0)

    val viewNames = getShortNames()
    val viewNameSel     = ".view_name *"    #> viewNames
    val shortNamesSel   = ".short_name *"   #> viewNames
    val aliasSel        = ".alias *"        #> getAliases()
    val descriptionSel  = ".description *"  #> getDescriptions()
    val isPublicSel     = ".is_public *"    #> getIfIsPublic()

    val permissionNames = permissions.keys
    val permSel = ".permissions *" #>
      permissionNames.map(
        permName => {
          ".permission_name *"  #> permName.capitalize.replace("_", " ") &
          ".permission_value *" #> getPermissionValues(permName)
        }
      )

      (viewNameSel & shortNamesSel & aliasSel & descriptionSel & isPublicSel & permSel).apply(xhtml)
     }


    def getShortNames() :List[String] = {
      views.map( view => view.shortName.getOrElse(""))
    }

    def getAliases() :List[String] = {
     views.map( view => view.alias.getOrElse(""))
    }

    def getDescriptions() :List[String] = {
      views.map( view => view.description.getOrElse(""))
    }

    def getIfIsPublic() :List[CssSel] = {
      views.map(
        view => {
          val isPublic = view.isPublic.getOrElse(false)
          val checkBox =
            if(isPublic)
              ".is_public_cb [checked]" #> "checked"
            else
              ".is_public_cb [name]" #> "is_public"
          checkBox
        }
      )
    }

    def getPermissionValues(permName: String) :List[CssSel] = {
      views.map(
        view => {
          val permValue: Boolean = view.permissions(permName)
          val checkBox =
            if(permValue){
              ".permission_value_cb [checked]" #> "checked" &
               ".permission_value_cb [value]" #> permName &
               ".permission_value_cb [name]" #> permName
            }
            else{
              ".permission_value_cb [value]" #> permName &
               ".permission_value_cb [name]" #> permName
            }

          checkBox
        }
      )
    }
  }