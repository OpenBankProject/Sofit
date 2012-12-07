package code.model.traits
import java.util.Date
import net.liftweb.common.Box
import net.liftweb.json.JsonAST.JObject
import net.liftweb.json.JsonDSL._

trait Comment {
  def id_ : String
  // The person that posted the comment
  def postedBy : Box[User] 
  
  //the id of the view related to the comment 
  def viewId : Long

  // The actual text of the comment
  def text : String
  
  def datePosted : Date
  
  def toJson : JObject = {
    ("id" -> "") ~
    ("comment" -> text) ~
    ("view" -> viewId) ~
    ("user_provider" -> "") ~
    ("user_id" -> "") ~
    ("user_name" -> postedBy.map(_.emailAddress).getOrElse("")) ~
    ("reply_to" -> "")
  }
}