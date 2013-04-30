package code.api.v1_2

import org.scalatest._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import _root_.net.liftweb.util._
import Helpers._
import _root_.net.liftweb.common._
import dispatch._
import oauth._
import OAuth._
import _root_.net.liftweb.json.Extraction
import _root_.net.liftweb.json.Serialization
import _root_.net.liftweb.json.Serialization.{read, write}
import dispatch.liftjson.Js._
import _root_.net.liftweb.json.JsonAST.{JValue, JObject}
import org.mortbay.jetty.nio.SelectChannelConnector
import net.liftweb.json.NoTypeHints
import net.liftweb.json.JsonDSL._

import code.model.{Consumer => OBPConsumer, Token => OBPToken}
import code.model.TokenType
import code.api.test.{ServerSetup, APIResponse}

@RunWith(classOf[JUnitRunner])
class API1_2Test extends ServerSetup{

  val v1_2Request = baseRequest / "obp" / "v1.2"

  //create the application
  lazy val testConsumer =
    OBPConsumer.create.
      name("test application").
      isActive(true).
      key(Helpers.randomString(40).toLowerCase).
      secret(Helpers.randomString(40).toLowerCase).
      saveMe

  lazy val consumer = new Consumer (testConsumer.key,testConsumer.secret)
  // create the access token
  lazy val tokenDuration = Helpers.weeks(4)
  lazy val testToken =
    OBPToken.create.
    tokenType(TokenType.Access).
    consumerId(testConsumer.id).
    userId("1").
    key(Helpers.randomString(40).toLowerCase).
    secret(Helpers.randomString(40).toLowerCase).
    duration(tokenDuration).
    expirationDate({(now : TimeSpan) + tokenDuration}).
    insertDate(now).
    saveMe

  lazy val token = new Token(testToken.key, testToken.secret)

  /********************* API test methods ********************/
  def getAPIInfo : h.HttpPackage[APIResponse] = {
    val request = v1_2Request
    makeGetRequest(request)
  }

  def getBanksInfo : h.HttpPackage[APIResponse]  = {
    val request = v1_2Request / "banks"
    makeGetRequest(request)
  }

  val emptyJSON : JObject =
    ("" -> "")

  def getPublicAccounts : h.HttpPackage[APIResponse]= {
    val reply = getBanksInfo
    val banksInfo = reply.body.extract[BanksJSON]
    if(! banksInfo.banks.isEmpty)
    {
      val bank = banksInfo.banks.head
      val request = v1_2Request / "banks" / bank.bank.id / "accounts" / "public"
      makeGetRequest(request)
    }
    else
      new APIResponse(400,emptyJSON)
  }

  def getPrivateAccounts : h.HttpPackage[APIResponse] = {
    val reply = getBanksInfo
    val banksInfo = reply.body.extract[BanksJSON]
    if(! banksInfo.banks.isEmpty)
    {
      val bank = banksInfo.banks.head
      val request = v1_2Request / "banks" / bank.bank.id / "accounts" / "private" <@(consumer,token)
      makeGetRequest(request)
    }
    else
      new APIResponse(400,emptyJSON)
  }

  /************************ the tests ************************/
  feature("base line URL works"){
    scenario("we get the api information") {
      Given("The user is not logged in")
      When("the request is sent")
      val reply = getAPIInfo
      Then("we should get a 200 ok code")
      reply.code should equal (200)
      val apiInfo = reply.body.extract[APIInfoJSON]
      apiInfo.api.version should equal ("1.2")
    }
  }

  feature("Information about the hosted banks"){
    scenario("we get the hosted banks information") {
      Given("The user is not logged in")
      When("the request is sent")
      val reply = getBanksInfo
      Then("we should get a 200 ok code")
      reply.code should equal (200)
      val banksInfo = reply.body.extract[BanksJSON]
    }
  }

  feature("Information about the public bank accounts"){
    scenario("we get the public bank accounts") {
       Given("The user is not logged in")
       When("the request is sent")
       val reply = getPublicAccounts
       Then("we should get a 200 ok code")
       reply.code should equal (200)
       val publicAccountsInfo = reply.body.extract[AccountsJSON]
       println("public accounts : " + publicAccountsInfo)
    }
  }

  feature("Information about the private bank accounts"){
    scenario("we get the private bank accounts") {
       Given("The we will use an access token")
       When("the request is sent")
       val reply = getPrivateAccounts
       Then("we should get a 200 ok code")
       reply.code should equal (200)
       val privateAccountsInfo = reply.body.extract[AccountsJSON]
       println("private accounts : " + privateAccountsInfo)
    }
  }

}
