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
import scala.util.Random

import code.model.{Consumer => OBPConsumer, Token => OBPToken}
import code.model.TokenType
import code.api.test.{ServerSetup, APIResponse}
import code.model.dataAccess.OBPUser

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

  // create an other user for test purposes
  lazy val user =
    OBPUser.create.
      email(Helpers.randomString(5) + "@exemple.com").
      password(Helpers.randomString(10)).
      validated(true).
      firstName(Helpers.randomString(10)).
      lastName(Helpers.randomString(10)).
      saveMe

  //we create an access token for the other user
  lazy val testToken2 =
    OBPToken.create.
    tokenType(TokenType.Access).
    consumerId(testConsumer.id).
    userId(user.id.get.toString).
    key(Helpers.randomString(40).toLowerCase).
    secret(Helpers.randomString(40).toLowerCase).
    duration(tokenDuration).
    expirationDate({(now : TimeSpan) + tokenDuration}).
    insertDate(now).
    saveMe

  lazy val token2 = new Token(testToken2.key, testToken2.secret)

  /********************* API test methods ********************/
  val emptyJSON : JObject =
    ("error" -> "empty List")
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

  def getBankInfoWithRandomID : h.HttpPackage[APIResponse]  = {
      val request = v1_2Request / "banks" / Helpers.randomString(5)
      makeGetRequest(request)
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

  case class BankAccountPermission(
    response : APIResponse,
    bank : String = "",
    account : String = ""
  )

  def getAccountPermission : BankAccountPermission = {
    val accounts = getBankAccounts
    val accountsInfo = accounts.body.extract[AccountsJSON]
    if(accountsInfo.accounts.nonEmpty)
    {
      val accountsSize = accountsInfo.accounts.size
      val randomAccount = Random.nextInt(accountsSize)
      val account = accountsInfo.accounts(randomAccount)
      val request = v1_2Request / "banks" / account.bank_id / "accounts" / account.id / "users" <@(consumer,token)
      val response = makeGetRequest(request)
      new BankAccountPermission(response, account.bank_id, account.id)
    }
    else
      new BankAccountPermission(errorAPIResponse)
  }

  def getAccountPermissionWithoutToken : h.HttpPackage[APIResponse]= {
    val accounts = getBankAccounts
    val accountsInfo = accounts.body.extract[AccountsJSON]
    if(accountsInfo.accounts.nonEmpty)
    {
      val accountsSize = accountsInfo.accounts.size
      val randomAccount = Random.nextInt(accountsSize)
      val account = accountsInfo.accounts(randomAccount)
      val request = v1_2Request / "banks" / account.bank_id / "accounts" / account.id / "users"
      makeGetRequest(request)
    }
    else
      errorAPIResponse
  }

  def getAccountPermissionWithoutOwnerAccess : h.HttpPackage[APIResponse]= {
    val accounts = getBankAccounts
    val accountsInfo = accounts.body.extract[AccountsJSON]
    if(accountsInfo.accounts.nonEmpty)
    {
      val accountsSize = accountsInfo.accounts.size
      val randomAccount = Random.nextInt(accountsSize)
      val account = accountsInfo.accounts(randomAccount)
      val request = v1_2Request / "banks" / account.bank_id / "accounts" / account.id / "users" <@(consumer,token2)
      makeGetRequest(request)
    }
    else
      errorAPIResponse
  }

  def getUserAccountPermission : h.HttpPackage[APIResponse]= {
    val permissions = getAccountPermission
    val permissionsInfo = permissions.response.body.extract[PermissionsJSON]
    if(permissionsInfo.permissions.nonEmpty){
      val permissionsSize = permissionsInfo.permissions.size
      val randomPermission = Random.nextInt(permissionsSize)
      val permission = permissionsInfo.permissions(randomPermission)
      val request = v1_2Request / "banks" / permissions.bank / "accounts" / permissions.account / "users"/ permission.user.id <@(consumer,token)
      makeGetRequest(request)
    }
    else
      errorAPIResponse
  }

  def getUserAccountPermissionWithoutToken : h.HttpPackage[APIResponse]= {
    val permissions = getAccountPermission
    val permissionsInfo = permissions.response.body.extract[PermissionsJSON]
    if(permissionsInfo.permissions.nonEmpty){
      val permissionsSize = permissionsInfo.permissions.size
      val randomPermission = Random.nextInt(permissionsSize)
      val permission = permissionsInfo.permissions(randomPermission)
      val request = v1_2Request / "banks" / permissions.bank / "accounts" / permissions.account / "users"/ permission.user.id
      makeGetRequest(request)
    }
    else
      errorAPIResponse
  }

  def getUserAccountPermissionWithRandomUserId : h.HttpPackage[APIResponse]= {
    val permissions = getAccountPermission
    val permissionsInfo = permissions.response.body.extract[PermissionsJSON]
    if(permissionsInfo.permissions.nonEmpty){
      val randomUserId = Helpers.randomString(10)
      val request = v1_2Request / "banks" / permissions.bank / "accounts" / permissions.account / "users"/ randomUserId <@(consumer,token)
      makeGetRequest(request)
    }
    else
      errorAPIResponse
  }

  def grantUserAccessToView : h.HttpPackage[APIResponse] = {
    val accountInfo = getPrivateBankAccountDetails.body.extract[ModeratedAccountJSON]
    //Note: for the moment we have a limited number of views, so the following list contains permalinks of all the views except Full, Base and Public.
    val possibleViewsPermalinks = List("team", "board", "authorities", "our-network", "owner", "management")
    val randomPosition = Random.nextInt(possibleViewsPermalinks.size)
    val view = possibleViewsPermalinks(randomPosition)
    println("==>should be 201")
    println("==>granting access to user id: " + user.id.get.toString)
    val request = (v1_2Request / "banks" / accountInfo.bank_id / "accounts" / accountInfo.id / "users"/ user.id.get.toString / "views" / view).POST.<@(consumer,token)
    makePostRequest(request)
  }

  def grantUserAccessToViewWithRandomUserID : h.HttpPackage[APIResponse] = {
    val accountInfo = getPrivateBankAccountDetails.body.extract[ModeratedAccountJSON]
    //Note: for the moment we have a limited number of views, so the following list contains permalinks of all the views except Full, Base and Public.
    val possibleViewsPermalinks = List("team", "board", "authorities", "our-network", "owner", "management")
    val randomPosition = Random.nextInt(possibleViewsPermalinks.size)
    val view = possibleViewsPermalinks(randomPosition)
    val request = (v1_2Request / "banks" / accountInfo.bank_id / "accounts" / accountInfo.id / "users"/ Helpers.randomString(10) / "views" / view).POST.<@(consumer,token)
    makePostRequest(request)
  }

  def grantUserAccessToViewWithRandomView : h.HttpPackage[APIResponse] = {
    val accountInfo = getPrivateBankAccountDetails.body.extract[ModeratedAccountJSON]
    val request = (v1_2Request / "banks" / accountInfo.bank_id / "accounts" / accountInfo.id / "users"/ user.id.get.toString / "views" / Helpers.randomString(4)).POST.<@(consumer,token)
    makePostRequest(request)
  }

  def grantUserAccessToViewWithoutOwnerAccess : h.HttpPackage[APIResponse] = {
    val accountInfo = getPrivateBankAccountDetails.body.extract[ModeratedAccountJSON]
    //Note: for the moment we have a limited number of views, so the following list contains permalinks of all the views except Full, Base and Public.
    val possibleViewsPermalinks = List("team", "board", "authorities", "our-network", "owner", "management")
    val randomPosition = Random.nextInt(possibleViewsPermalinks.size)
    val view = possibleViewsPermalinks(randomPosition)
    val request = (v1_2Request / "banks" / accountInfo.bank_id / "accounts" / accountInfo.id / "users"/ "1" / "views" / view).POST.<@(consumer,token2)
    makePostRequest(request)
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
      val bankInfo = reply.body.extract[BankJSON]
      bankInfo.id.nonEmpty should equal (true)
    }

    scenario("we don't get the hosted bank information") {
      Given("We will not use an access token and request a random bankId")
      When("the request is sent")
      val reply = getBankInfoWithRandomID
      Then("we should get a 400 code")
      reply.code should equal (400)
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
        a.views_available.foreach(
          //check that all the views are public
          v => v.is_public should equal (true)
        )
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
        a.views_available.foreach(
          //check that all the views are public
          v => v.is_public should equal (true)
        )
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
  feature("Information about the permissions of a specific bank account"){

    scenario("we will get one bank account permissions by using an access token") {
      Given("We will use an access token")
      When("the request is sent")
      val reply = getAccountPermission.response
      Then("we should get a 200 ok code")
      reply.code should equal (200)
      reply.body.extract[PermissionsJSON]
    }

    scenario("we will not get one bank account permissions") {
      Given("We will not use an access token")
      When("the request is sent")
      val reply = getAccountPermissionWithoutToken
      Then("we should get a 400 code")
      reply.code should equal (400)
    }

    scenario("we will not get one bank account permissions by using an other access token") {
      Given("We will use an access token, but that does not grant owner view")
      When("the request is sent")
      val reply = getAccountPermissionWithoutOwnerAccess
      Then("we should get a 400 code")
      reply.code should equal (400)
    }
  }

  feature("Information about the permissions of a specific user on a specific bank account"){
    scenario("we will get the permissions by using an access token") {
      Given("We will use an access token")
      When("the request is sent")
      val reply = getUserAccountPermission
      Then("we should get a 200 ok code")
      reply.code should equal (200)
      val viewsInfo = reply.body.extract[ViewsJSON]
      viewsInfo.views.foreach(v => v.id.nonEmpty should equal (true))
    }

    scenario("we will not get the permissions of a specific user") {
      Given("We will not use an access token")
      When("the request is sent")
      val reply = getUserAccountPermissionWithoutToken
      Then("we should get a 400 code")
      reply.code should equal (400)
    }

    scenario("we will not get the permissions of a random user") {
      Given("We will use an access token with random user id")
      When("the request is sent")
      val reply = getUserAccountPermissionWithRandomUserId
      Then("we should get a 400 code")
      reply.code should equal (400)
    }
  }

  feature("Grant a user access to a view on a bank account"){
    scenario("we will grant a user access to a view on an bank account") {
      Given("We will use an access token")
      When("the request is sent")
      val reply = grantUserAccessToView
      Then("we should get a 201 ok code")
      reply.code should equal (201)
      val viewInfo = reply.body.extract[ViewJSON]
      viewInfo.id.nonEmpty should equal (true)
    }

    scenario("we cannot grant a user access to a view on an bank account because the user does not exist") {
      Given("We will use an access token with a random user Id")
      When("the request is sent")
      val reply = grantUserAccessToViewWithRandomUserID
      Then("we should get a 400 ok code")
      reply.code should equal (400)
    }

    scenario("we cannot grant a user access to a view on an bank account because the view does not exist") {
      Given("We will use an access token")
      When("the request is sent")
      val reply = grantUserAccessToViewWithRandomView
      Then("we should get a 400 ok code")
      reply.code should equal (400)
    }

    scenario("we cannot grant a user access to a view on an bank account because the user does not have owner view access") {
      Given("We will use an access token")
      When("the request is sent")
      val reply = grantUserAccessToViewWithoutOwnerAccess
      Then("we should get a 400 ok code")
      reply.code should equal (400)
    }
  }
}
