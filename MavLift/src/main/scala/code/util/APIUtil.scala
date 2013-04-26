package code.util

import code.model.dataAccess.APIMetric
import net.liftweb.common.Full
import net.liftweb.util.Helpers._
import net.liftweb.http.S

object APIUtil {

  def httpMethod : String =
    S.request match {
      case Full(r) => r.request.method
      case _ => "GET"
    }

  def isThereAnOAuthHeader : Boolean = {
    S.request match {
      case Full(a) =>  a.header("Authorization") match {
        case Full(parameters) => parameters.contains("OAuth")
        case _ => false
      }
      case _ => false
    }
  }

  def logAPICall =
    APIMetric.createRecord.
      url(S.uriAndQueryString.getOrElse("")).
      date((now: TimeSpan)).
      save

  def gitCommit : String = {
    val commit = tryo{
      val properties = new java.util.Properties()
      properties.load(getClass().getClassLoader().getResourceAsStream("git.properties"))
      properties.getProperty("git.commit.id", "")
    }
    commit getOrElse ""
  }
}
