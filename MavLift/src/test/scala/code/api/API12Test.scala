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
  val emptyJSON : JObject =
    ("" -> "")
  val errorAPIResponse = new APIResponse(400,emptyJSON)

  def getAPIInfo : h.HttpPackage[APIResponse] = {
    val request = v1_2Request
    makeGetRequest(request)
  }

  def getBanksInfo : h.HttpPackage[APIResponse]  = {
    val request = v1_2Request / "banks"
    makeGetRequest(request)
  }

  def getBankInfo : h.HttpPackage[APIResponse]  = {
    val banksInfo = getBanksInfo.body.extract[BanksJSON]
    if(banksInfo.banks.nonEmpty)
    {
      val bank = banksInfo.banks.head
      val request = v1_2Request / "banks" / bank.id
      makeGetRequest(request)
    }
    else
      errorAPIResponse
  }

  def getPublicAccounts : h.HttpPackage[APIResponse]= {
    val reply = getBanksInfo
    val banksInfo = reply.body.extract[BanksJSON]
    if(banksInfo.banks.nonEmpty)
    {
      val bank = banksInfo.banks.head
      val request = v1_2Request / "banks" / bank.id / "accounts" / "public"
      makeGetRequest(request)
    }
    else
      errorAPIResponse
  }

  def getPrivateAccounts : h.HttpPackage[APIResponse] = {
    val reply = getBanksInfo
    val banksInfo = reply.body.extract[BanksJSON]
    if(banksInfo.banks.nonEmpty)
    {
      val bank = banksInfo.banks.head
      val request = v1_2Request / "banks" / bank.id / "accounts" / "private" <@(consumer,token)
      makeGetRequest(request)
    }
    else
      errorAPIResponse
  }

  def getBankAccounts : h.HttpPackage[APIResponse] = {
    val reply = getBanksInfo
    val banksInfo = reply.body.extract[BanksJSON]
    if(banksInfo.banks.nonEmpty)
    {
      val bank = banksInfo.banks.head
      val request = v1_2Request / "banks" / bank.id / "accounts"
      makeGetRequest(request)
    }
    else
      errorAPIResponse
  }

  def getBankAccountsWithToken : h.HttpPackage[APIResponse] = {
    val reply = getBanksInfo
    val banksInfo = reply.body.extract[BanksJSON]
    if(banksInfo.banks.nonEmpty)
    {
      val bank = banksInfo.banks.head
      val request = v1_2Request / "banks" / bank.id / "accounts" <@(consumer,token)
      makeGetRequest(request)
    }
    else
      errorAPIResponse
  }

  def getPrivateAccountsWithOutToken : h.HttpPackage[APIResponse] = {
    val reply = getBanksInfo
    val banksInfo = reply.body.extract[BanksJSON]
    if(banksInfo.banks.nonEmpty)
    {
      val bank = banksInfo.banks.head
      val request = v1_2Request / "banks" / bank.id / "accounts" / "private"
      makeGetRequest(request)
    }
    else
      errorAPIResponse
  }

  def getPublicBankAccountDetails : h.HttpPackage[APIResponse] = {
    val reply = getPublicAccounts
    val accountsInfo = reply.body.extract[AccountsJSON]
    if(accountsInfo.accounts.nonEmpty)
    {
      val account = accountsInfo.accounts.head
      val view = account.views_available.head
      val request = v1_2Request / "banks" / account.bank_id / "accounts" / account.id / view.id / "account"
      makeGetRequest(request)
    }
    else
      errorAPIResponse
  }

  def getPrivateBankAccountDetails : h.HttpPackage[APIResponse] = {
    val reply = getPrivateAccounts
    val accountsInfo = reply.body.extract[AccountsJSON]
    if(accountsInfo.accounts.nonEmpty)
    {
      val account = accountsInfo.accounts.head
      val view = account.views_available.head
      val request = v1_2Request / "banks" / account.bank_id / "accounts" / account.id / view.id / "account" <@(consumer,token)
      makeGetRequest(request)
    }
    else
      errorAPIResponse
  }

  /************************ the tests ************************/
  feature("base line URL works"){
    scenario("we get the api information") {
      Given("We will not use an access token")
      When("the request is sent")
      val reply = getAPIInfo
      Then("we should get a 200 ok code")
      reply.code should equal (200)
      val apiInfo = reply.body.extract[APIInfoJSON]
      apiInfo.version should equal ("1.2")
      apiInfo.git_commit.nonEmpty should equal (true)
    }
  }

  feature("Information about the hosted banks"){
    scenario("we get the hosted banks information") {
      Given("We will not use an access token")
      When("the request is sent")
      val reply = getBanksInfo
      Then("we should get a 200 ok code")
      reply.code should equal (200)
      val banksInfo = reply.body.extract[BanksJSON]
      banksInfo.banks.foreach(b => {
        b.id.nonEmpty should equal (true)
      })
    }
  }

  feature("Information about one hosted bank"){
    scenario("we get the hosted bank information") {
      Given("We will not use an access token")
      When("the request is sent")
      val reply = getBankInfo
      Then("we should get a 200 ok code")
      reply.code should equal (200)
      val banksInfo = reply.body.extract[BankJSON]
      banksInfo.id.nonEmpty should equal (true)
    }
  }

  feature("Information about all the bank accounts"){
    scenario("we get only the public bank accounts") {
      Given("We will not use an access token")
      When("the request is sent")
      val reply = getBankAccounts
      Then("we should get a 200 ok code")
      reply.code should equal (200)
      val publicAccountsInfo = reply.body.extract[AccountsJSON]
      publicAccountsInfo.accounts.foreach(a => {
        a.id.nonEmpty should equal (true)
        a.views_available.nonEmpty should equal (true)
      })
    }

    scenario("we get the bank accounts the user have access to") {
      Given("We will use an access token")
      When("the request is sent")
      val reply = getBankAccountsWithToken
      Then("we should get a 200 ok code")
      reply.code should equal (200)
      val accountsInfo = reply.body.extract[AccountsJSON]
      accountsInfo.accounts.foreach(a => {
        a.id.nonEmpty should equal (true)
        a.views_available.nonEmpty should equal (true)
      })
    }
  }

  feature("Information about the public bank accounts"){
    scenario("we get the public bank accounts") {
      Given("We will not use an access token")
      When("the request is sent")
      val reply = getPublicAccounts
      Then("we should get a 200 ok code")
      reply.code should equal (200)
      val publicAccountsInfo = reply.body.extract[AccountsJSON]
      publicAccountsInfo.accounts.foreach(a => {
        a.id.nonEmpty should equal (true)
        a.views_available.nonEmpty should equal (true)
      })
    }
  }

  feature("Information about the private bank accounts"){
    scenario("we get the private bank accounts") {
      Given("We will use an access token")
      When("the request is sent")
      val reply = getPrivateAccounts
      Then("we should get a 200 ok code")
      reply.code should equal (200)
      val privateAccountsInfo = reply.body.extract[AccountsJSON]
      privateAccountsInfo.accounts.foreach(a => {
        a.id.nonEmpty should equal (true)
        a.views_available.nonEmpty should equal (true)
      })
    }
    scenario("we don't get the private bank accounts") {
      Given("We will not use an access token")
      When("the request is sent")
      val reply = getPrivateAccountsWithOutToken
      Then("we should get a 400 code")
      reply.code should equal (400)
    }
  }

  feature("Information about a bank account"){
    scenario("we get data without using an access token") {
      Given("We will not use an access token")
      When("the request is sent")
      val reply = getPublicBankAccountDetails
      Then("we should get a 200 ok code")
      reply.code should equal (200)
      val publicAccountDetails = reply.body.extract[ModeratedAccountJSON]
      publicAccountDetails.id.nonEmpty should equal (true)
      publicAccountDetails.bank_id.nonEmpty should equal (true)
      publicAccountDetails.views_available.nonEmpty should equal (true)
    }

    scenario("we get data by using an access token") {
      Given("We will use an access token")
      When("the request is sent")
      val reply = getPrivateBankAccountDetails
      Then("we should get a 200 ok code")
      reply.code should equal (200)
      val privateAccountDetails = reply.body.extract[ModeratedAccountJSON]
      privateAccountDetails.id.nonEmpty should equal (true)
      privateAccountDetails.bank_id.nonEmpty should equal (true)
      privateAccountDetails.views_available.nonEmpty should equal (true)
    }
  }
}
