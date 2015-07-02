package code.snippet

import _root_.net.liftweb._
import code.lib.ObpGet

import http._
import util._
import _root_.scala.xml.{NodeSeq, Text}


import net.liftweb._
import http._
import common._


import net.liftweb.json.Extraction._


import net.liftweb.util.Helpers._
import net.liftweb.http.SHtml.{text,ajaxSubmit}
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds.SetHtml
import xml.Text


/*
Present a list of OBP resource URLs
 */
class LiveDocs {
  def showResources = {
    case class Resource(verb: String,
                        url: String,
                        description: String,
                        representation: String)

    val resources = List(
      Resource("GET", "/v1.4.0/banks", "Get banks on this server", "JSON"),
      Resource("GET", "/v1.4.0/banks/BANK_ID", "Get a particular bank identified by its ID", "JSON"),
      Resource("GET", "/v1.4.0/banks/BANK_ID/branches", "Get branches of a certain bank", "JSON"))

    ".resource" #> resources.map { i =>
      ".resource-verb" #> i.verb &
        ".resource-url" #> i.url &
        ".resource-description" #> i.description &
        ".resource-representation" #> i.representation
    }
  }
}


/*
Call an OBP URL and return the response to the browser in JSON form.
 */
object ApiCallForm extends Loggable {


  def getResponse (url : String ) : String = {

    val responseBodyBox = ObpGet(url)
    logger.info(s"responseBodyBox is ${responseBodyBox}")


    implicit val formats = net.liftweb.json.DefaultFormats

    // extractOpt ?
    val responseBody = decompose(responseBodyBox)


    import net.liftweb.json.Serialization.writePretty


    val jsonString = writePretty(responseBody)


    logger.info(s"jsonString is $jsonString")

    jsonString

  }


  def render = {

      var urlToCall = ""

      // /v1.4.0/banks

      def process(): JsCmd = {
        SetHtml("result", Text(getResponse(urlToCall)))
      }

      // form field (on the left) is bound to the variable (urlToCall)
      // (However, updating the var here does not seeem to update the form field value)
      "@url_to_call" #> text(urlToCall, s => urlToCall = s) &
        // Replace the type=submit with Javascript that makes the ajax call.
        "type=submit" #> ajaxSubmit("Call OBP API", process)

  }
}