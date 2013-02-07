package code.model.traits

import java.util.Date
import net.liftweb.common.Box
import java.net.URL

trait TransactionImage {

  def id_ : String
 
  def datePosted : Date

  def postedBy : Box[User]

  def viewId : Long 

  def description : String
  
  def imageUrl : URL
  
}