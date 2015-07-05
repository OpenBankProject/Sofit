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
    case class Resource(id: String,
                        verb: String,
                        url: String,
                        description: String,
                        representation: String)

    val resources = List(
      Resource("1", "GET", "/v1.4.0/banks", "Get banks on this server", "JSON"),
      Resource("2", "GET", "/v1.4.0/banks/BANK_ID", "Get a particular bank identified by its ID", "JSON"),
      Resource("3", "GET", "/v1.4.0/banks/BANK_ID/branches", "Get branches of a certain bank", "JSON"))





  // Notes on below. To have a $ in the resulting string use two: $$
  // Can't escape " with \" or use triple quoted string in the string interpolation so use the replace hack


    ".resource" #> resources.map { i =>
      ".resource_verb" #> i.verb &
      ".resource_url" #> i.url &
      ".resource_description" #> i.description &
      ".resource_representation" #> i.representation &
      ".resource_url_td [id]" #> s"resource_url_td_${i.id}" &
      ".url_caller [id]" #> s"url_caller_${i.id}" &
      ".url_to_call [id]" #> s"url_to_call_${i.id}" &
      ".try_me_button [onclick]" #> s"$$(DOUBLE-QUOTE#url_caller_${i.id}DOUBLE-QUOTE).fadeToggle(); $$(DOUBLE-QUOTE#url_to_call_${i.id}DOUBLE-QUOTE).val($$(DOUBLE-QUOTE#resource_url_td_${i.id}DOUBLE-QUOTE)[0].innerHTML);".replaceAll("DOUBLE-QUOTE",""""""")
    }
  }
}






/*
Call an OBP URL and return the response to the browser in JSON form.
 */
object CallMe extends Loggable {


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

      def process(): JsCmd = {
        SetHtml("result", Text(getResponse(urlToCall)))
      }

      // form field (on the left) is bound to the variable (urlToCall)
      // (However, updating the var here does not seem to update the form field value)
      "@url_to_call" #> text(urlToCall, s => urlToCall = s) &
        // Replace the type=submit with Javascript that makes the ajax call.
        "type=submit" #> ajaxSubmit("Go", process)

  }
}