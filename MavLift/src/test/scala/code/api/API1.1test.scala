package code.api

import org.scalatest._
import org.scalatest.FeatureSpec
import org.scalatest.{FunSpec, BeforeAndAfterAll}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import _root_.net.liftweb.mapper._
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import _root_.net.liftweb.http._
import _root_.org.mortbay.jetty.Connector
import _root_.org.mortbay.jetty.Server
import _root_.org.mortbay.jetty.webapp.WebAppContext
import dispatch._
import _root_.net.liftweb.json.Extraction
import _root_.net.liftweb.json.Serialization
import _root_.net.liftweb.json.Serialization.{read, write}
import dispatch.liftjson.Js._
import _root_.net.liftweb.json.JsonAST.JValue
import org.mortbay.jetty.nio.SelectChannelConnector
import net.liftweb.json.NoTypeHints


case class APIResponse(code: Int, body: JValue)


@RunWith(classOf[JUnitRunner])
class API1_1Tests extends FeatureSpec 
  with BeforeAndAfter with GivenWhenThen 
  with ShouldMatchers with BeforeAndAfterAll {

  /**
  * test server settings
  */
  val host = "localhost"
  val port = 8000
  val server = new Server
  val scc = new SelectChannelConnector
  scc.setPort(port)
  server.setConnectors(Array(scc))

  val context = new WebAppContext()
  context.setServer(server)
  context.setContextPath("/")
  context.setWar("src/main/webapp")

  server.addHandler(context)
  implicit val formats = Serialization.formats(NoTypeHints)

  val h = new Http with thread.Safety
  val baseRequest = (:/(host, Integer.valueOf(port)))
  val v1_1Request = baseRequest / "obp" / "1.1"

  /**
  * the methods lunched before all the tests
  */
  override def beforeAll() {
    server.start()
  }

  /**
  * the methods lunched after all the tests
  */    
  override def afterAll() {
    server.stop()
  }

  /**
  * this method do a post request given a URL, a JSON and an optional Headers Map 
  */
  def makePostRequest(req: Request, json: String, headers : Map[String,String] = Map()) : h.HttpPackage[APIResponse] = {
    val jsonReq = req << (json, "application/json") <:< headers
    val jsonHandler = jsonReq ># {json => json}
    h x jsonHandler{
       case (code, _, _, json) => APIResponse(code, json())
    }
  }

  def makePutRequest(req: Request, json: String, headers : Map[String,String] = Map()) : h.HttpPackage[APIResponse] = {
    val jsonReq = req <<< json <:< headers 
    val jsonHandler = jsonReq ># {json => json}
    h x jsonHandler{
       case (code, _, _, json) => APIResponse(code, json())
    }
  }

  /**
  * this method do a post request given a URL 
  */
  def makeGetRequest(req: Request, headers : Map[String,String] = Map()) : h.HttpPackage[APIResponse] = {
    val jsonReq = req <:< headers
    val jsonHandler = jsonReq ># {json => json}
    h x jsonHandler{
       case (code, _, _, json) => APIResponse(code, json())
    }
  }
  
  def getAPIInfo = {
    val request = v1_1Request / "azezae"
    makeGetRequest(request)
  }

  /************************ the tests ************************/
  feature("base line URL works"){
    
    scenario("we get the api information") {
       Given("The user is not logged in")
       When("the request is sent")
       val reply = getAPIInfo
       Then("we should get a 200 created code")
       reply.code should equal (200)

    }
  }
}       
