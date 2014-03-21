package code.snippet

import net.liftweb.util.Helpers._

import code.lib.ObpJson.ViewJson

class ViewsOverview(views : List[ViewJson]) {
	def showAll = {
		"#listedViews" #> views.toString()
	}
}