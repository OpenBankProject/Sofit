package code.snippet

import _root_.net.liftweb._
import code.lib.ObpJson.ResourceDoc
import code.lib.{ObpPut, ObpDelete, ObpPost, ObpGet}
//import code.snippet.CallUrlForm._
import net.liftweb.http.{SHtml, S}

import net.liftweb.json.{Extraction, JsonParser, JsonAST}
import net.liftweb.json.JsonAST.{JField, JObject, JValue}
import scala.xml.{XML, NodeSeq, Text}


import net.liftweb._
// for compact render
import net.liftweb.json._


import common._

import net.liftweb.util.Helpers._
import net.liftweb.http.SHtml.{text,ajaxSubmit, textarea}
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds.{Run, SetHtml}

import net.liftweb.json.Serialization.writePretty

import code.lib.ObpAPI.getResourceDocsJson

import net.liftweb.markdown._

/*
Present a list of OBP resource URLs
 */
class ApiExplorer extends Loggable {

  def stringToNodeSeq(html : String) : NodeSeq = {
    scala.xml.XML.loadString("<div>" + html + "</div>")
  }


  def showResources = {

    // Get the requested version from the url parameter and default if none
    val apiVersionRequested = S.param("api-version").getOrElse("1.4.0")

    if (apiVersionRequested != "1.4.0") S.notice("Note: Only 1.4.0 is currently supported")

    logger.info (s"API version requested is: $apiVersionRequested")

    // Get a list of resource docs from the API server
    // This will throw an exception if resource_docs key is not populated
    // Convert the json representation to ResourceDoc (pretty much a one to one mapping)


    // The overview contains html. Just need to convert it to a NodeSeq so the template will render it as such
    val resources = for {
      r <- getResourceDocsJson.map(_.resource_docs).get
    } yield ResourceDoc(id = r.operation_id, verb = r.request_verb, url = r.request_url, summary = r.summary, description = stringToNodeSeq(r.description), example_request_body = r.example_request_body)




    // Controls when we display the request body.
    def displayRequestBody(resourceVerb : String) = {
      resourceVerb match {
        case "POST" => "block"
        case "PUT" => "block"
        case "PATCH" => "block"
        case _ => "none"
      }
    }


    // Constant
    val apiVersion = "v1.4.0"

    var resourceId = ""
    var requestVerb = ""
    var requestUrl = ""
    var requestBody = "{}"
    var sOverView = "" // not used

    def process(): JsCmd = {
      logger.info(s"requestUrl is $requestUrl")
      logger.info(s"resourceId is $resourceId")
      logger.info(s"requestBody is $requestBody")


      // Create json object from input string
      val jsonObject = JsonParser.parse(requestBody).asInstanceOf[JObject]

      // the id of the element we want to populate and format.
      val target = "result_" + resourceId

      // This will highlight the json. Replace the $ sign after we've constructed the string
      val jsCommand : String =  s"DOLLAR_SIGN('#$target').each(function(i, block) { hljs.highlightBlock(block);});".replace("DOLLAR_SIGN","$")

      logger.info(s"command is $jsCommand")

      // Return the commands to call the url with optional body and put the response into the appropriate result div
      SetHtml(target, Text(getResponse(apiVersion, requestUrl, requestVerb, jsonObject))) &
      Run (jsCommand)
    }


    def getResponse (apiVersion : String, url : String, resourceVerb: String, json : JValue) : String = {

      implicit val formats = net.liftweb.json.DefaultFormats

      val urlWithVersion = s"/$apiVersion$url"

      val responseBodyBox = {
        resourceVerb match {
          case "GET" => ObpGet(urlWithVersion)
          case "DELETE" => ObpDelete(urlWithVersion)
          case "POST" => ObpPost(urlWithVersion, json)
          case "PUT" => ObpPut(urlWithVersion, json)
          case _ => {
            val failMsg = s"Live Docs says: Unsupported resourceVerb: $resourceVerb. Url requested was: $url"
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
          case Failure(message, _, _) => message
        }

      logger.info(s"responseBody is $responseBody")
      responseBody
    }

    // In case we use Extraction.decompose
    implicit val formats = net.liftweb.json.DefaultFormats

    // Below we render the resources into a (nested) table.
    // Notes on escaping strings:
    // To have a $ in the resulting string use two: $$
    // Can't escape " with \" or use triple quoted string in the string interpolation so may need to use the replace hack

    // Note: This is the return of the function.
    // All the replacements you want to do *must be chained here together at the end of the function*.
    // Also, you can't use "replaceWith" (the alias for #>) to chain

    // See the following for some examples.
    // http://blog.knoldus.com/2013/03/08/lift-web-basics-of-using-snippets-for-a-beginner/
    // http://simply.liftweb.net/index-7.10.html

    // Show the version to the user.
    // Append to the content child of id="version" e.g. the fixed text "Version:" is replacedWith "Version: 1.2.3"
    "#version *+" #> apiVersion &
    // replace the node identified by the class "resource" with the following
    // This creates the list of resources in the DOM
    ".resource" #> resources.map { i =>
      ".resource_summary *" #> i.summary &
      ".resource_summary [href]" #> s"#${i.id}" &
      ".resource_summary [name]" #> s"${i.id}" &
      // Replace attribute named overview_text with the value (whole div/span element is replaced leaving just the text)
      "@description_text" #> i.description &
      "@resource_description [id]" #> s"description_${i.id}" &
      ".resource_url_td [id]" #> s"resource_url_td_${i.id}" &   // Probably don't need this now
      ".resource_verb_td [id]" #> s"resource_verb_td_${i.id}" & // Probably don't need this now
      ".url_caller [id]" #> s"url_caller_${i.id}" &
      // ".try_me_button [onclick]" #> s"$$(DOUBLE-QUOTE#url_caller_${i.id}DOUBLE-QUOTE).fadeToggle();".replaceAll("DOUBLE-QUOTE",""""""") &
      ".result [id]" #> s"result_${i.id}" &
      "@example_request_body [id]" #> s"example_request_body_${i.id}" &
      "@example_request_body [style]" #> s"display: ${displayRequestBody(i.verb)};" &
      //////
      // The form field (on the left) is bound to the variable (urlToCall)
      // (However, updating the var here does not seem to update the form field value)
      // We provide a default value (i.url) and bind the user input to requestUrl. requestURL is available in the function process
      // text creates a text box and we can capture its input in requestUrl
      "@request_url_input" #> text(i.url, s => requestUrl = s, "maxlength" -> "255", "size" -> "100", "id" -> s"request_url_input_${i.id}") &
      // Extraction.decompose creates json representation of JObject.
      "@example_request_body_input" #> text(pretty(render(i.example_request_body)), s => requestBody = s, "maxlength" -> "255", "size" -> "100", "type" -> "text") &
      // TODO get this working. requestBody is not populated with textarea value "@request_body_input" #> textarea(pretty(render(i.example_request_body)), s => requestBody = s, "cols" -> "90", "rows" -> "5") &
      // We're not using the id at the moment
      "@request_verb_input" #> text(i.verb, s => requestVerb = s, "type" -> "hidden", "id" -> s"request_verb_input_${i.id}") &
      "@resource_id_input" #> text(i.id.toString, s => resourceId = s, "type" -> "hidden", "id" -> s"resource_id_input_${i.id}") &
      // Replace the type=submit with Javascript that makes the ajax call.
      // The button. First argument is the text of the button (GET, POST etc). Second argument is function to call. Arguments to the func could be sent in third argument
      "@success_response_body [id]" #> s"success_response_body_${i.id}" &
      ".call_button" #> ajaxSubmit(i.verb, process)
    }
  }
}