package code.snippet

import _root_.net.liftweb._
import code.lib.APIUtils._
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

  // Render the resources into a (nested) table.
  // This could probably be improved.
  // So far we can't (why??) set value of input field at render time like this ".resource_id [value]" #> s"lslslsls${i.id}"
  // so have to work around and set the input fields via the try_me_button onclick javascript.
  // Notes on escaping strings
  // To have a $ in the resulting string use two: $$
  // Can't escape " with \" or use triple quoted string in the string interpolation so use the replace hack


    ".resource" #> resources.map { i =>
      ".resource_verb" #> i.verb &
      ".resource_url" #> i.url &
      ".resource_description" #> i.description &
      ".resource_representation" #> i.representation &
      ".resource_url_td [id]" #> s"resource_url_td_${i.id}" &
      ".resource_verb_td [id]" #> s"resource_verb_td_${i.id}" &
      ".url_caller [id]" #> s"url_caller_${i.id}" &
      ".url_to_call [id]" #> s"url_to_call_${i.id}" &
      ".url_to_call [value]" #> s"${i.url}" &
      ".try_me_button [onclick]" #> s"$$(DOUBLE-QUOTE#url_caller_${i.id}DOUBLE-QUOTE).fadeToggle(); $$(DOUBLE-QUOTE#url_to_call_${i.id}DOUBLE-QUOTE).val($$(DOUBLE-QUOTE#resource_url_td_${i.id}DOUBLE-QUOTE)[0].innerHTML); $$(DOUBLE-QUOTE#resource_id_${i.id}DOUBLE-QUOTE).val(DOUBLE-QUOTE${i.id}DOUBLE-QUOTE); $$(DOUBLE-QUOTE#resource_verb_input_${i.id}DOUBLE-QUOTE).val($$(DOUBLE-QUOTE#resource_verb_td_${i.id}DOUBLE-QUOTE)[0].innerHTML);".replaceAll("DOUBLE-QUOTE",""""""") &
      ".result [id]" #> s"result_${i.id}" &
      ".resource_id [id]" #> s"resource_id_${i.id}" &
      ".resource_verb_input [id]" #> s"resource_verb_input_${i.id}"
    }
  }
}


/*
Call an OBP URL and return the response to the browser in JSON form.
 */
object CallMe extends Loggable {


  def getResponse (url : String, resourceVerb: String) : String = {


    // TODO: Handle POST requests


    val responseBodyBox = {
      resourceVerb match {
        case "GET" => ObpGet(url)
        case _ => {
          val failMsg = s"Unsupported resourceVerb: $resourceVerb. Please use GET"
          logger.warn(failMsg)
          Failure(failMsg)
        }
      }
    }



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
      var resourceId = ""
      var resourceVerb = ""

      def process(): JsCmd = {
        SetHtml("result_" + resourceId, Text(getResponse(urlToCall, resourceVerb)))
      }

      // The form field (on the left) is bound to the variable (urlToCall)
      // (However, updating the var here does not seem to update the form field value)
      // TODO Rename all these input fields _input

      "@url_to_call" #> text(urlToCall, s => urlToCall = s) &
      "@resource_id" #> text(resourceId, s => resourceId = s) &
      "@resource_verb_input" #> text(resourceVerb, s => resourceVerb = s) &
      // Replace the type=submit with Javascript that makes the ajax call.
      "type=submit" #> ajaxSubmit("Go", process)

  }
}