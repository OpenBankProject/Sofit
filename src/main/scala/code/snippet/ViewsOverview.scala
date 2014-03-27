package code.snippet

import net.liftweb.util.Helpers._
import scala.xml.NodeSeq

import code.lib.ObpJson.CompleteViewJson

class ViewsOverview(views : List[CompleteViewJson]) {

	def showAll(xhtml: NodeSeq) : NodeSeq ={

		views.flatMap {
		  view => {
		    val id = view.id
		    val shortName = view.shortName
		    val description = view.description

				val viewId = ".view_name [id]" #> id
				val viewShortName = ".view_name *" #> shortName
    		val viewDescription = ".view_description *" #> description

    	 (viewId & viewShortName & viewDescription).apply(xhtml)
		  }
    }
	}


}


    