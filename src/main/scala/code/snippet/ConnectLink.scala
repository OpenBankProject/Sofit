package code.snippet

import net.liftweb.util.Helpers._
import net.liftweb.util.Props

class ConnectLink {

  def link = "* [href]" #> Props.get("api_hostname").map(_ + "/connect").getOrElse("#")
  
}