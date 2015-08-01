package code.snippet

import _root_.net.liftweb._
import code.lib.ObpJson.ResourceDoc
import code.lib.{ObpPost, ObpGet}
import net.liftweb.http.S

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

import code.lib.ObpAPI.getResourceDocsJson

/*
Present a list of OBP resource URLs
 */
class LiveDocs extends Loggable {
  def showResources = {
    case class Resource(id: String,
                        verb: String,
                        url: String,
                        description: String,
                        representation: String)

    // Get the requested version from the url parameter and default if none
    val apiVersion = S.param("api-version").getOrElse("1.4.0")

    if (apiVersion != "1.4.0") S.notice("Note: Only 1.4.0 is currently supported")

    logger.info (s"API version requested is: $apiVersion")

    // Get a list of resource docs from the API server
    // This will throw exception if resource_docs key is not populated
    // Convert the json representation to ResourceDoc (pretty much a one to one mapping)
    val resources = for {
      r <- getResourceDocsJson.map(_.resource_docs).get
    } yield ResourceDoc(id = r.id, verb = r.verb, url = r.url, description = r.description)


  // Render the resources into a (nested) table.
  // This could probably be improved.
  // So far we can't (why?) set value of input field at render time like this ".resource_id_input [value]" #> s"lslslsls${i.id}"
  // so have to work around and set the input fields via the try_me_button onclick javascript.
  // Notes on escaping strings
  // To have a $ in the resulting string use two: $$
  // Can't escape " with \" or use triple quoted string in the string interpolation so use the replace hack


    def displayBody(resourceVerb : String) = {
      resourceVerb match {
        case "POST" => "block"
        case _ => "none"
      }
    }


    // Note! This is the return of the function.
    // All the replacements you want to do *must be chained here together at the end of the function*.
    // Also, you can't chain replaceWith (alias for #>)

    // See the following for some examples.
    // http://blog.knoldus.com/2013/03/08/lift-web-basics-of-using-snippets-for-a-beginner/
    // http://simply.liftweb.net/index-7.10.html

    // Show the version to the user.
    // Append to the content child of id="version" i.e. Version: -> Version: 1.2.3
    "#version *+" #> apiVersion &
    // replace the node identified by the class "resource" with the following
    // This creates the list of resources in the DOM
    ".resource" #> resources.map { i =>
      ".resource_verb" #>  i.verb &
      ".resource_url" #> i.url &
      ".resource_description" #> i.description &
      ".resource_representation" #> "JSON" &
      ".resource_url_td [id]" #> s"resource_url_td_${i.id}" &
      ".resource_verb_td [id]" #> s"resource_verb_td_${i.id}" &
      ".url_caller [id]" #> s"url_caller_${i.id}" &
      "@request_url_input [id]" #> s"request_url_input_${i.id}" &
      "@request_url_input [value]" #> s"${i.url}" &
      ".try_me_button [onclick]" #> s"$$(DOUBLE-QUOTE#url_caller_${i.id}DOUBLE-QUOTE).fadeToggle();  $$(DOUBLE-QUOTE#request_url_input_${i.id}DOUBLE-QUOTE).val($$(DOUBLE-QUOTE#resource_url_td_${i.id}DOUBLE-QUOTE)[0].innerHTML); $$(DOUBLE-QUOTE#resource_id_input_${i.id}DOUBLE-QUOTE).val(DOUBLE-QUOTE${i.id}DOUBLE-QUOTE); $$(DOUBLE-QUOTE#request_verb_input_${i.id}DOUBLE-QUOTE).val($$(DOUBLE-QUOTE#resource_verb_td_${i.id}DOUBLE-QUOTE)[0].innerHTML);".replaceAll("DOUBLE-QUOTE",""""""") &
      ".result [id]" #> s"result_${i.id}" &
      "@resource_id_input [id]" #> s"resource_id_input_${i.id}" &
      "@request_verb_input [id]" #> s"request_verb_input_${i.id}" &
      "@request_body [id]" #> s"request_body_${i.id}" &
      "@request_body [style]" #> s"display: ${displayBody(i.verb)};"
    }
  }
}


/*
Call an OBP URL and return the response to the browser in JSON form.
*/
object CallUrlForm extends Loggable {


  def getResponse (apiVersion : String, url : String, resourceVerb: String, json : JValue) : String = {

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

      var apiVersion = "/obp/v1.4.0/"
      var resourceId = ""
      var requestVerb = ""
      var requestUrl = ""
      var requestBody = "{}"

      def process(): JsCmd = {
        // Create json object from input string
        val jsonObject = JsonParser.parse(requestBody).asInstanceOf[JObject]
        // Call the url with optional body and put the response into the appropriate result div
        SetHtml("result_" + resourceId, Text(getResponse(apiVersion, requestUrl, requestVerb, jsonObject))) &
        // This applies json highlighting to the json
        Run ("$('pre code').each(function(i, block) { hljs.highlightBlock(block);});")
      }

      // The form field (on the left) is bound to the variable (urlToCall)
      // (However, updating the var here does not seem to update the form field value)
      "@resource_id_input" #> text(resourceId, s => resourceId = s, "type" -> "hidden") &
      "@request_verb_input" #> text(requestVerb, s => requestVerb = s, "type" -> "hidden") &
      "@request_url_input" #> text(requestUrl, s => requestUrl = s, "maxlength" -> "255", "size" -> "100") &
      "@request_body_input" #> text(requestBody, s => requestBody = s, "type" -> "text") &
      // Replace the type=submit with Javascript that makes the ajax call.
      "type=submit" #> ajaxSubmit("Call", process)

  }
}