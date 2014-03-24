package code.snippet

import net.liftweb.util.Helpers._
import scala.xml.NodeSeq

import code.lib.ObpJson.ViewJson

class ViewsOverview(views : List[ViewJson]) {

	def showAll(xhtml: NodeSeq) : NodeSeq = {
		"#listedViews" #> views.toString()

		views.flatMap {
		  view => {
		    val id = view.id.getOrElse("")
		    val short_name = view.short_name.getOrElse("")
		    val description = view.description.getOrElse("")

				val viewId = ".view_name [id]" #> id
				val viewShortName = ".view_name *" #> short_name
        val viewDescription = ".view_description *" #> description

       (viewId &
        viewShortName &
        viewDescription).apply(xhtml)
		  }
    }
	}


}


    