package code.snippet

import net.liftweb.util.Helpers._
import scala.xml.NodeSeq
import net.liftweb.util.CssSel
import net.liftweb.mapper.By

import code.lib.ObpJson.CompleteViewJson


class ViewsOverview(views : List[CompleteViewJson]) {

  def getTableContent(xhtml: NodeSeq) :NodeSeq = {

    val permissionsCollection: List[Map[String, Any]] = views.map(view => view.permissions)
    val permissions: Map[String, Any] = permissionsCollection(0)

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

    def getIfIsPublic() :List[String] = {
      views.map( view => view.isPublic.getOrElse("").toString())
    }

    def getPermissionValues(permName: String) :List[String] = {
      views.map( view => view.permissions(permName).toString())
    }
  }