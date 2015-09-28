package code.snippet

import _root_.net.liftweb._
import code.lib.ObpJson.ResourceDoc
import code.lib.{ObpPut, ObpDelete, ObpPost, ObpGet}
//import code.snippet.CallUrlForm._
import net.liftweb.http.S

import net.liftweb.json.{Extraction, JsonParser, JsonAST}
import net.liftweb.json.JsonAST.{JField, JObject, JValue}
import _root_.scala.xml.{NodeSeq, Text}


import net.liftweb._
// for compact render
import net.liftweb.json._


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
class ApiExplorer extends Loggable {
  def showResources = {

    // Get the requested version from the url parameter and default if none
    val apiVersionRequested = S.param("api-version").getOrElse("1.4.0")

    if (apiVersionRequested != "1.4.0") S.notice("Note: Only 1.4.0 is currently supported")

    logger.info (s"API version requested is: $apiVersionRequested")

    // Get a list of resource docs from the API server
    // This will throw exception if resource_docs key is not populated
    // Convert the json representation to ResourceDoc (pretty much a one to one mapping)


    val resources = for {
      r <- getResourceDocsJson.map(_.resource_docs).get
    } yield ResourceDoc(id = r.id, verb = r.request_verb, url = r.request_url, description = r.description, request_body = r.request_body)


  // Render the resources into a (nested) table.
  // Notes on escaping strings:
  // To have a $ in the resulting string use two: $$
  // Can't escape " with \" or use triple quoted string in the string interpolation so use the replace hack



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

    def process(): JsCmd = {
      logger.info(s"requestUrl is $requestUrl")
      logger.info(s"resourceId is $resourceId")


      // Create json object from input string
      val jsonObject = JsonParser.parse(requestBody).asInstanceOf[JObject]
      // Call the url with optional body and put the response into the appropriate result div

      // the id of the element we want to populate and format.
      val target = "result_" + resourceId

      // This will highlight the json. Replace the $ sign after we've constructed the string
      val jsCommand : String =  s"DOLLAR_SIGN('#$target').each(function(i, block) { hljs.highlightBlock(block);});".replace("DOLLAR_SIGN","$")

      logger.info(s"command is $jsCommand")

      // Return the commands
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


    // Note! This is the return of the function.
    // All the replacements you want to do *must be chained here together at the end of the function*.
    // Also, you can't use replaceWith (the alias for #>) to chain

    // See the following for some examples.
    // http://blog.knoldus.com/2013/03/08/lift-web-basics-of-using-snippets-for-a-beginner/
    // http://simply.liftweb.net/index-7.10.html

    // Show the version to the user.
    // Append to the content child of id="version" e.g. the fixed text "Version:" is replacedWith "Version: 1.2.3"
    "#version *+" #> apiVersion &
    // replace the node identified by the class "resource" with the following
    // This creates the list of resources in the DOM
    ".resource" #> resources.map { i =>
      //".resource_verb" #>  i.verb &
      //".resource_url" #> i.url &
      ".resource_description" #> i.description &
      ".resource_url_td [id]" #> s"resource_url_td_${i.id}" &   // Probably don't need this now
      ".resource_verb_td [id]" #> s"resource_verb_td_${i.id}" & // Probably don't need this now
      ".url_caller [id]" #> s"url_caller_${i.id}" &
      // ".try_me_button [onclick]" #> s"$$(DOUBLE-QUOTE#url_caller_${i.id}DOUBLE-QUOTE).fadeToggle();".replaceAll("DOUBLE-QUOTE",""""""") &
      ".result [id]" #> s"result_${i.id}" &
      "@request_body [id]" #> s"request_body_${i.id}" &
      "@request_body [style]" #> s"display: ${displayRequestBody(i.verb)};" &
      //////
      // The form field (on the left) is bound to the variable (urlToCall)
      // (However, updating the var here does not seem to update the form field value)
      // TODO use this approach.
      // We provide a default value (i.url) and bind the user input to requestUrl. requestURL is available in the function process
      "@request_url_input" #> text(i.url, s => requestUrl = s, "maxlength" -> "255", "size" -> "100", "id" -> s"request_url_input_${i.id}") &
      // Extraction.decompose creates json representation of JObject.
      "@request_body_input" #> text(pretty(render(i.request_body)), s => requestBody = s, "maxlength" -> "255", "size" -> "100", "type" -> "text") &
      // We're not using the id at the moment
      "@request_verb_input" #> text(i.verb, s => requestVerb = s, "type" -> "hidden", "id" -> s"request_verb_input_${i.id}") &
      "@resource_id_input" #> text(i.id.toString, s => resourceId = s, "type" -> "hidden", "id" -> s"resource_id_input_${i.id}") &
      // Replace the type=submit with Javascript that makes the ajax call.
      // The button. First argument is the text of the button (GET, POST etc). Second argument is function to call. Arguments to the func could be sent in third argument
      "@response_body [id]" #> s"response_body_${i.id}" &
      ".call_button" #> ajaxSubmit(i.verb, process)
    }
  }
}