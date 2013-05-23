package code.api.test

import org.scalatest._
import dispatch._
import dispatch.liftjson.Js._
import net.liftweb.json.NoTypeHints
import net.liftweb.json.JsonAST.{JValue, JObject}
import net.liftweb.common._
import org.mortbay.jetty.Connector
import org.mortbay.jetty.Server
import org.mortbay.jetty.nio.SelectChannelConnector
import org.mortbay.jetty.webapp.WebAppContext
import net.liftweb.json.Serialization
import org.junit.runner.RunWith

case class APIResponse(code: Int, body: JValue)

trait ServerSetup extends FeatureSpec
  with BeforeAndAfter with GivenWhenThen
  with ShouldMatchers with BeforeAndAfterAll
  with Loggable{

  val server = ServerSetup

  implicit val formats = Serialization.formats(NoTypeHints)

  val h = new Http with thread.Safety
  val baseRequest = (:/(server.host, Integer.valueOf(server.port)))

  /**
  * the methods lunched before all the tests
  */
  override def beforeAll() {

  }

  /**
  * the methods lunched after all the tests
  */
  override def afterAll() {
  }

  /**
   this method do a post request given a URL, a JSON and an optional Headers Map
  */
  def makePostRequest(req: Request, json: String = "", headers : Map[String,String] = Map()) : h.HttpPackage[APIResponse] = {
    val jsonReq = req << (json, "application/json") <:< headers
    val jsonHandler = jsonReq ># {json => json}
    h x jsonHandler{
       case (code, _, _, json) => APIResponse(code, json())
    }
  }

  def makePutRequest(req: Request, json: String, headers : Map[String,String] = Map(("Content-type","application/json"))) : h.HttpPackage[APIResponse] = {
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

  /**
  * this method do a delete request given a URL
  */
  def makeDeleteRequest(req: Request, headers : Map[String,String] = Map()) : h.HttpPackage[APIResponse] = {
    val jsonReq = (req <:< headers).DELETE
    val jsonHandler = jsonReq.>|
    h x jsonHandler{
       case (code, _, _, _) => APIResponse(code, null)
    }
  }
}

object ServerSetup {

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

  server.start()
}