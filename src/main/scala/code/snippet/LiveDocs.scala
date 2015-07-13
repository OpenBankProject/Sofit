package code.snippet

import _root_.net.liftweb._
import code.lib.{ObpPost, ObpGet}

import net.liftweb.json.{JsonParser, JsonAST}
import net.liftweb.json.JsonAST.{JField, JObject, JValue}
import _root_.scala.xml.{NodeSeq, Text}


import net.liftweb._
import common._

import net.liftweb.util.Helpers._
import net.liftweb.http.SHtml.{text,ajaxSubmit}
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds.{Run, SetHtml}
import xml.Text

import net.liftweb.json.Serialization.writePretty


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
  // So far we can't (why??) set value of input field at render time like this ".resource_id_input [value]" #> s"lslslsls${i.id}"
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
      "@request_url_input [id]" #> s"request_url_input_${i.id}" &
      "@request_url_input [value]" #> s"${i.url}" &
      ".try_me_button [onclick]" #> s"$$(DOUBLE-QUOTE#url_caller_${i.id}DOUBLE-QUOTE).fadeToggle(); $$(DOUBLE-QUOTE#request_url_input_${i.id}DOUBLE-QUOTE).val($$(DOUBLE-QUOTE#resource_url_td_${i.id}DOUBLE-QUOTE)[0].innerHTML); $$(DOUBLE-QUOTE#resource_id_input_${i.id}DOUBLE-QUOTE).val(DOUBLE-QUOTE${i.id}DOUBLE-QUOTE); $$(DOUBLE-QUOTE#request_verb_input_${i.id}DOUBLE-QUOTE).val($$(DOUBLE-QUOTE#resource_verb_td_${i.id}DOUBLE-QUOTE)[0].innerHTML);".replaceAll("DOUBLE-QUOTE",""""""") &
      ".result [id]" #> s"result_${i.id}" &
      "@resource_id_input [id]" #> s"resource_id_input_${i.id}" &
      "@request_verb_input [id]" #> s"request_verb_input_${i.id}"
    }
  }
}


/*
Call an OBP URL and return the response to the browser in JSON form.
 */
object CallUrlForm extends Loggable {


  def getResponse (url : String, resourceVerb: String, json : JValue) : String = {


    implicit val formats = net.liftweb.json.DefaultFormats



    // TODO: Handle POST requests
    val responseBodyBox = {
      resourceVerb match {
        case "GET" => ObpGet(url)
        case "POST" => ObpPost(url, json)
        case _ => {
          val failMsg = s"Live Docs says: Unsupported resourceVerb: $resourceVerb."
          logger.warn(failMsg)
          Failure(failMsg)
        }
      }
    }


    logger.info(s"responseBodyBox is ${responseBodyBox}")

    // Handle the contents of the Box
    val responseBody =
      responseBodyBox match {
      case Full(json) => writePretty(json)
      case Empty => "Empty: API did not return anything"
      case Failure(message, _, _) => "Failure: " + message
    }


    logger.info(s"responseBody is $responseBody")
    responseBody
  }


  def render = {

      var resourceId = ""
      var requestUrl = ""
      var requestVerb = ""
      var requestBody = "{}"

      def process(): JsCmd = {
        // Create json object from input string
        val jsonObject = JsonParser.parse(requestBody).asInstanceOf[JObject]
        // Call the url with optional body and put the response into the appropriate result div
        SetHtml("result_" + resourceId, Text(getResponse(requestUrl, requestVerb, jsonObject))) &
        // This applies json highlighting to the json
        Run ("$('pre code').each(function(i, block) { hljs.highlightBlock(block);});")
      }

      // The form field (on the left) is bound to the variable (urlToCall)
      // (However, updating the var here does not seem to update the form field value)
      "@resource_id_input" #> text(resourceId, s => resourceId = s, "type" -> "hidden") &
      "@request_verb_input" #> text(requestVerb, s => requestVerb = s, "type" -> "hidden") &
      "@request_url_input" #> text(requestUrl, s => requestUrl = s, "maxlength" -> "255", "size" -> "100") &
      "@request_body_input" #> text(requestBody, s => requestBody = s) &
      // Replace the type=submit with Javascript that makes the ajax call.
      "type=submit" #> ajaxSubmit("Go", process)

  }
}