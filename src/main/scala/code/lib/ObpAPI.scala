package code.lib

import java.io._
import java.lang
import java.net.{HttpURLConnection, URL}
import java.security.Security
import java.text.SimpleDateFormat
import java.time.{ZoneId, ZonedDateTime}
import java.util.UUID.randomUUID
import java.util.{Date, UUID}

import code.Constant._
import code.lib.ObpJson.{CurrentUserJson, _}
import code.util.Helper.MdcLoggable
import com.nimbusds.jose.crypto.bc.BouncyCastleProviderSingleton
import com.nimbusds.jose.crypto.{ECDSASigner, RSASSASigner}
import com.nimbusds.jose.jwk.{ECKey, JWK, RSAKey}
import com.nimbusds.jose.{JOSEException, JWSAlgorithm, JWSHeader, JWSSigner}
import com.nimbusds.jwt.{JWTClaimsSet, SignedJWT}
import net.liftweb.common._
import net.liftweb.http.RequestVar
import net.liftweb.json.JsonAST.JValue
import net.liftweb.json.JsonDSL._
import net.liftweb.json.{JObject, compactRender, _}
import net.liftweb.util.Helpers._
import net.liftweb.util.Props
import okhttp3.MediaType

import scala.collection.immutable.List
import scala.xml.NodeSeq

case class Header(key: String, value: String)

case class ErrorMessage(code: Int,
                        message: String
                       )

object ObpAPI extends MdcLoggable {
  
  implicit val formats = DefaultFormats
  val dateFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSZ")
  val dateFormatWithoutMilis = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'")
  
  val defaultProvider = Props.get("defaultAuthProvider").getOrElse("")
  
  val userNotFoundError = "user (\\S+) at provider (\\S+) not found".r
  
  /**
   * The request vars ensure that for one page load, the same API call isn't
   * made multiple times
   */
  object allBanksVar extends RequestVar[Box[BanksJson]] (Empty)
  
  def allBanks : Box[BanksJson]= {
    allBanksVar.get match {
      case Full(a) => Full(a)
      case _ => ObpGet(s"/$versionOfApi/banks").flatMap(_.extractOpt[BanksJson])
    }
  }
  
  trait SortDirection {
    val value : String
  }
  object ASC extends SortDirection { val value = "ASC" }
  object DESC extends SortDirection { val value = "DESC" }

  def currentUser : Box[CurrentUserJson]= if(OAuthClient.loggedIn){
    ObpGet(s"/v2.0.0/users/current").flatMap(_.extractOpt[CurrentUserJson])
  } else Failure("OBP-20001: User not logged in. Authentication is required!")
  
  
  def getUserCustomerLinksByUserId(userId: String) : Box[UserCustomerLinksJson]= {
    ObpGet(s"/$versionOfApi/user_customer_links/users/$userId").flatMap(_.extractOpt[UserCustomerLinksJson])
  }  
  def getUserCustomerLinksByCustomerId(customerId: String) : Box[UserCustomerLinksJson]= {
    ObpGet(s"/$versionOfApi/user_customer_links/customers/$customerId").flatMap(_.extractOpt[UserCustomerLinksJson])
  }  

  def createUserCustomerLink(bankId: String, userId: String, customerId: String): Box[UserCustomerLinkJson] = {
    val json =
      CreateUserCustomerLinkJson(
        user_id = userId,
        customer_id = customerId
      )
    val result = ObpPost(s"/$versionOfApi/banks/" + urlEncode(bankId) + "/user_customer_links", Extraction.decompose(json))
    result.flatMap(_.extractOpt[UserCustomerLinkJson])
  }
  def createUserCustomerLinkIfDoesNotExists(bankId: String, userId: String, customerId: String): Boolean = {
    // TODO refactor this so that we check if there are links first instead of relying on the error message.
    createUserCustomerLink(bankId, userId, customerId) match {
      case Full(_) => true
      // This means that the userId is already linked to the customerId at the bankId
      case Failure(msg, _, _) if msg.contains("OBP-30007") => true
      case _ => false
    }
  }
  def getUserCustomerLink(bankId: String, userId: String, customerId: String): Box[UserCustomerLinkJson] = {
    ObpGet(s"/$versionOfApi/banks/" + urlEncode(bankId) + "/user_customer_links/users/" + userId)
      .flatMap(_.extractOpt[UserCustomerLinksJson])
      .flatMap(_.user_customer_links.filter(_.customer_id == customerId).headOption)
  }

  def getCustomersForCurrentUser() : Box[CustomersWithAttributesJsonV300]= {
    ObpGet(s"/$versionOfApi/users/current/customers").flatMap(_.extractOpt[CustomersWithAttributesJsonV300])
  }
  
  def getMyCorrelatedEntities: Box[CorrelatedEntities]= {
    ObpGet(s"/$versionOfApi/my/correlated-entities").flatMap(_.extractOpt[CorrelatedEntities])
  }  
  def createCurrentUserAttribute(name: String,
                                 `type`: String,
                                 value: String): Box[UserAttributeResponseJsonV400]= {
    val json = UserAttributeJsonV400(name: String, `type`: String, value: String)
    val response = ObpPost(s"/$versionOfApi/my/user/attributes", Extraction.decompose(json))
      .flatMap(_.extractOpt[UserAttributeResponseJsonV400])
    response
  }  
  def updateCurrentUserAttribute(userAttributeId: String,
                                 name: String,
                                 `type`: String,
                                 value: String): Box[UserAttributeResponseJsonV400]= {
    val json = UserAttributeJsonV400(name: String, `type`: String, value: String)
    val response = ObpPut(s"/$versionOfApi/my/user/attributes/$userAttributeId", Extraction.decompose(json))
      .flatMap(_.extractOpt[UserAttributeResponseJsonV400])
    response
  }  
  def getCurrentUserAttributes(): Box[UserAttributesResponseJson]= {
    val response = ObpGet(s"/$versionOfApi/my/user/attributes")
      .flatMap(_.extractOpt[UserAttributesResponseJson])
    response
  }
  
  def getOrCreateCustomer(bankId: String, legalName: String) : Box[String]= {
    logger.debug(s"hello from getOrCreateCustomer bankId is: $bankId, legalName is: $legalName")
    getCustomersForCurrentUser() match {
      case Full(list) => {
        logger.debug("getOrCreateCustomer found a list of Customers for current User")
        list.customers.find(_.legal_name == legalName) match {
          case Some(customer) => {
            logger.debug("getOrCreateCustomer found at least one customer with matching legalName:" + legalName)
            Full(customer.customer_id)
          }
          case None => {
            logger.debug("getOrCreateCustomer did not find a customer with matching legal name so will createCustomer for: " + legalName)
            createCustomer(bankId, legalName).map(_.customer_id)}
      }}
      case Empty => {
        logger.debug("getOrCreateCustomer did not find any Customers for the current User so will createCustomer")
        createCustomer(bankId, legalName).map(_.customer_id)}
      case ParamFailure(msg,_,_,_) =>
        throw new Exception(msg)
      case obj@Failure(msg, _, _) =>
        throw new Exception(msg)
      case _ =>
        throw new Exception("Unknown Error.")
    }
  }
  def createCustomer(bankId: String, legal_name: String): Box[CustomerJsonV310] = {
    val json =
      PostCustomerJsonV310(
        legal_name = legal_name,
        mobile_phone_number = "",
        email = "",
        face_image = CustomerFaceImageJson("", new Date()),
        date_of_birth = new Date(),
        relationship_status = "",
        dependants = 0,
        dob_of_dependants = List(),
        credit_rating = CustomerCreditRatingJSON("",""),
        credit_limit = AmountOfMoneyJsonV121("",""),
        highest_education_attained = "",
        employment_status = "",
        kyc_status = true,
        last_ok_date = new Date(),
        title  = "",
        branch_id = "",
        name_suffix = ""
      )
    val result = ObpPost(s"/$versionOfApi/banks/" + urlEncode(bankId) + "/customers", Extraction.decompose(json))
    result.flatMap(_.extractOpt[CustomerJsonV310])
  }

  def createRandomUser() : Box[GetUserJsonV400] = {
  //  extends MdcLoggable

    //create a random username.
    val randomUserName = randomUUID().toString.replace("-", "")
    logger.debug("setupCorrelatedUser says randomUserName is: " + randomUserName)

    val randomLongStringPassword = randomUUID().toString

    val json =
      PostUserJsonV400(
        email = randomUserName + "@example.com",
        username = randomUserName,
        password = randomLongStringPassword,
        first_name = randomUserName,
        last_name = randomUserName
      )
    val result = ObpPost(s"/$versionOfApi/users", Extraction.decompose(json))
    result.flatMap(_.extractOpt[GetUserJsonV400])

  }




  /**
   * @return Json for transactions of a particular bank account
   */
  def transactions(bankId: String, accountId: String, viewId: String, limit: Option[Int],
      offset: Option[Int], fromDate: Option[Date], toDate: Option[Date], sortDirection: Option[SortDirection]) : Box[TransactionsJson]= {

    val headers : List[Header] = limit.map(l => Header("obp_limit", l.toString)).toList ::: offset.map(o => Header("obp_offset", o.toString)).toList :::
      fromDate.map(f => Header("obp_from_date", dateFormat.format(f))).toList ::: toDate.map(t => Header("obp_to_date", dateFormat.format(t))).toList :::
      sortDirection.map(s => Header("obp_sort_direction", s.value)).toList ::: Nil

    ObpGet(s"/$versionOfApi/banks/" + urlEncode(bankId) + "/accounts/" + urlEncode(accountId) + "/" + urlEncode(viewId) +
              "/transactions", headers).flatMap(x => x.extractOpt[TransactionsJson])
  }



  /**
   * @return Json for transactions of a particular bank account Uses 1.2.1 call and format.
   */
  def transactions121(bankId: String, accountId: String, viewId: String, limit: Option[Int],
                   offset: Option[Int], fromDate: Option[Date], toDate: Option[Date], sortDirection: Option[SortDirection]) : Box[TransactionsJson121]= {

    val headers : List[Header] = limit.map(l => Header("obp_limit", l.toString)).toList ::: offset.map(o => Header("obp_offset", o.toString)).toList :::
      fromDate.map(f => Header("obp_from_date", dateFormat.format(f))).toList ::: toDate.map(t => Header("obp_to_date", dateFormat.format(t))).toList :::
      sortDirection.map(s => Header("obp_sort_direction", s.value)).toList ::: Nil

    ObpGet(s"/$versionOfApi/banks/" + urlEncode(bankId) + "/accounts/" + urlEncode(accountId) + "/" + urlEncode(viewId) +
      "/transactions", headers).flatMap(x => x.extractOpt[TransactionsJson121])
  }







  def publicAccounts(bankId : String) : Box[BarebonesAccountsJson] = {
    ObpGet(s"/$versionOfApi/banks/" + urlEncode(bankId) + "/accounts/public").flatMap(_.extractOpt[BarebonesAccountsJson])
  }

  def publicAccounts : Box[BarebonesAccountsJson] = {
    ObpGet(s"/$versionOfApi/accounts/public").flatMap(_.extractOpt[BarebonesAccountsJson])
  }

  def privateAccounts(bankId : String) : Box[BarebonesAccountsJson] = {
    ObpGet(s"/$versionOfApi/banks/" + urlEncode(bankId) + "/accounts/private").flatMap(_.extractOpt[BarebonesAccountsJson])
  }

  def privateAccounts : Box[BarebonesAccountsJson] = {
    ObpGet(s"/$versionOfApi/my/accounts").flatMap(_.extractOpt[BarebonesAccountsJson])
  }

  def getAccountBalances(bankId : String, accountId: String) : Box[AccountBalanceJsonV400] = {
    ObpGet(s"/$versionOfApi/banks/" + urlEncode(bankId) + "/accounts/" + accountId + "/balances")
      .flatMap(_.extractOpt[AccountBalanceJsonV400])
  }
  def getAccountsBalances(bankId : String) : Box[AccountsBalancesJsonV400] = {
    ObpGet(s"/$versionOfApi/banks/" + urlEncode(bankId) + "/balances").flatMap(_.extractOpt[AccountsBalancesJsonV400])
  }

  def allAccountsAtOneBank(bankId : String) : Box[BarebonesAccountsJson] = {
    ObpGet(s"/$versionOfApi/banks/" + urlEncode(bankId) + "/accounts").flatMap(_.extractOpt[BarebonesAccountsJson])
  }

  // Similar to getViews below
  def getViewsForBankAccount(bankId: String, accountId: String) = {
    ObpGet(s"/$versionOfApi/banks/" + bankId + "/accounts/" + accountId + "/views").flatMap(_.extractOpt[ViewsJson])
  }

  def getAccount(bankId: String, accountId: String, viewId: String) : Box[AccountJson] = {
    OAuthClient.loggedIn match {
      case true => ObpGet(s"/$versionOfApi/banks/" + urlEncode(bankId) + "/accounts/" + urlEncode(accountId) + "/" + urlEncode(viewId) + "/account").flatMap(x => x.extractOpt[AccountJson])
      case _ => ObpGet(s"/$versionOfApi/banks/" + urlEncode(bankId) + "/public/accounts/" + urlEncode(accountId) + "/" + urlEncode(viewId) + "/account").flatMap(x => x.extractOpt[AccountJson])
    }
  }

  def getCounterparties(bankId: String, accountId: String, viewId: String): Box[DirectOtherAccountsJson] = {
    val counterparties  = ObpGet(s"/$versionOfApi/banks/" + urlEncode(bankId) + "/accounts/" + urlEncode(accountId) + "/" + urlEncode(viewId) + "/other_accounts").flatMap(x => x.extractOpt[DirectOtherAccountsJson])
    counterparties
  }


  def getBalancingTransaction(transactionId: String): Box[DoubleEntryTransactionJson] = {
    val result  = ObpGet(s"/$versionOfApi/transactions/" + transactionId + "/balancing-transaction")
      .flatMap(x => x.extractOpt[DoubleEntryTransactionJson])
    result
  }



  // Returns Json containing Resource Docs
  def getResourceDocsJson : Box[ResourceDocsJson] = {
    ObpGet(s"/$versionOfApi/resource-docs/obp").flatMap(_.extractOpt[ResourceDocsJson])
  }

  /**
   * @return True if the account was deleted
   */
  def deleteAccount(bankId : String, accountId : String) : Boolean  = {
    val deleteAccountUrl = "/internal/v1.0/banks/" + urlEncode(bankId) + "/accounts/" + urlEncode(accountId)
    ObpInternalDelete(deleteAccountUrl)
  }

  def updateAccountLabel(bankId: String, accountId : String, label: String) = {
    val json =
      ("id" -> accountId) ~
      ("label" -> label) ~
      ("bank_id" -> bankId)
    ObpPost(s"/$versionOfApi/banks/" + urlEncode(bankId) + "/accounts/" + urlEncode(accountId), json)
  }

  def createAccount(bankId: String, label: String, userId: String): Box[JValue] = {
    val json =
      CreateAccountRequestJsonV310(
        user_id = userId,
        label = label,
        product_code = "None",
        balance = AmountOfMoneyJsonV121(currency = "EUR", "0"),
        branch_id = "None",
        account_routings = Nil
      )
    ObpPost(s"/$versionOfApi/banks/" + urlEncode(bankId) + "/accounts", Extraction.decompose(json))
  }
  def createIncome(bankId: String, accountId: String, incomeDescription: String, incomeAmount: String, incomeCurrency: String): Box[PostHistoricalTransactionResponseJson] = {
    val incomeAccountId = Props.get("incoming.account_id", "")
    val utcZoneId = ZoneId.of("UTC")
    val zonedDateTime = ZonedDateTime.now
    val utcDateTime = zonedDateTime.withZoneSameInstant(utcZoneId)
    import java.time.format.DateTimeFormatter
    val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss'Z'")
    val json =
      PostHistoricalTransactionAtBankJson(
        from_account_id = incomeAccountId,
        to_account_id = accountId,
        value = AmountOfMoneyJsonV121(currency = incomeCurrency, amount = incomeAmount),
        description = incomeDescription,
        posted = utcDateTime.format(formatter),
        completed= utcDateTime.format(formatter),
        `type`= "SANDBOX_TAN",
        charge_policy= "SHARED"
      )

    ObpPost(s"/$versionOfApi/banks/$bankId/management/historical/transactions", Extraction.decompose(json))
      .flatMap(_.extractOpt[PostHistoricalTransactionResponseJson])
  }

  def createOutcome(bankId: String, accountId: String, outcomeDescription: String, outcomeAmount: String, outcomeCurrency: String): Box[PostHistoricalTransactionResponseJson] = {
    val outcomeAccountId = Props.get("outgoing.account_id", "outgoing.account_id")
    val utcZoneId = ZoneId.of("UTC")
    val zonedDateTime = ZonedDateTime.now
    val utcDateTime = zonedDateTime.withZoneSameInstant(utcZoneId)
    import java.time.format.DateTimeFormatter
    val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss'Z'")
    val json =
      PostHistoricalTransactionAtBankJson(
        from_account_id = accountId,
        to_account_id = outcomeAccountId,
        value = AmountOfMoneyJsonV121(currency = outcomeCurrency, amount = outcomeAmount),
        description = outcomeDescription,
        posted = utcDateTime.format(formatter),
        completed= utcDateTime.format(formatter),
        `type`= "SANDBOX_TAN",
        charge_policy= "SHARED"
      )
    ObpPost(s"/$versionOfApi/banks/$bankId/management/historical/transactions", Extraction.decompose(json))
      .flatMap(_.extractOpt[PostHistoricalTransactionResponseJson])
  }

   /**
   * @return The json for the attribute if it was successfully added
   */
  def addTransactionAttribute(bankId : String,
                              accountId : String,
                              transactionId: String,
                              name: String,
                              `type`: String,
                              value: String) : Box[TransactionAttributeResponseJson] = {

    val json = TransactionAttributeJsonV400(name = name, `type` = `type`, value = value)

    val url = s"/$versionOfApi/banks/" + urlEncode(bankId) + "/accounts/" + urlEncode(accountId) + "/" +
      "/transactions/" + urlEncode(transactionId) + "/attribute"

    ObpPost(url, Extraction.decompose(json)).flatMap(_.extractOpt[TransactionAttributeResponseJson])
  }
   /**
   * @return The json for the comment if it was successfully added
   */
  def addComment(bankId : String, accountId : String, viewId : String,
      transactionId: String, comment: String) : Box[TransactionCommentJson] = {

    val addCommentJson = ("value" -> comment)

    val addCommentUrl = s"/$versionOfApi/banks/" + urlEncode(bankId) + "/accounts/" + urlEncode(accountId) + "/" + urlEncode(viewId) +
      "/transactions/" + urlEncode(transactionId) + "/metadata/comments"

    ObpPost(addCommentUrl, addCommentJson).flatMap(_.extractOpt[TransactionCommentJson])
  }

  def addPermission(bankId: String, accountId: String, userId : String, viewId: String) = {
    val grantPermissionUrl = s"/$versionOfApi121/banks/" + urlEncode(bankId) + "/accounts/" + urlEncode(accountId) +
      "/permissions/" + urlEncode(defaultProvider) + "/" +urlEncode(userId) + "/views/" + urlEncode(viewId)
    ObpPost(grantPermissionUrl, new JObject(Nil))
  }

  def addPermissions(bankId: String, accountId: String, userId: String, viewIds : List[String]) : Box[JValue] = {
    val addPermissionsUrl = s"/$versionOfApi121/banks/" + urlEncode(bankId) + "/accounts/" +
         urlEncode(accountId) + "/permissions/" + urlEncode(defaultProvider) + "/" + urlEncode(userId) + "/views"
    val json = ("views" -> viewIds)

    for {
      result <- ObpPost(addPermissionsUrl, json)
    } yield result
  }

  def getPermissions(bankId: String, accountId : String) : Box[PermissionsJson] = {
    ObpGet(s"/$versionOfApi/banks/" + bankId + "/accounts/" + accountId + "/permissions").flatMap(x => x.extractOpt[PermissionsJson])
  }

  def removePermission(bankId: String, accountId: String, userId : String, viewId: String) = {
    val removePermissionUrl = s"/$versionOfApi121/banks/" + urlEncode(bankId) + "/accounts/" + urlEncode(accountId) + "/permissions/" +
      urlEncode(defaultProvider) + "/" + urlEncode(userId) + "/views/" + urlEncode(viewId)
    ObpDeleteBoolean(removePermissionUrl)
  }

  def removeAllPermissions(bankId: String, accountId: String, userId: String) = {
    val removeAllPermissionsUrl = s"/$versionOfApi121/banks/" + urlEncode(bankId) + "/accounts/" + urlEncode(accountId) + "/permissions/" +
      urlEncode(defaultProvider) + "/" + urlEncode(userId) + "/views"
    ObpDeleteBoolean(removeAllPermissionsUrl)
  }

  def getViews(bankId: String, accountId: String) : Box[List[ViewJson]] = {
    // Note function of similar name above
    for {
      json <- ObpGet(s"/$versionOfApi/banks/" + bankId + "/accounts/" + accountId + "/views")
      viewsJson <- Box(json.extractOpt[ViewsJson])
    } yield viewsJson.views.getOrElse(Nil)
  }

  def getCompleteViews(bankId: String, accountId: String) : Box[List[CompleteViewJson]] = {
    for {
      json <- ObpGet(s"/$versionOfApi/banks/" + bankId + "/accounts/" + accountId + "/views")
    } yield {
      json \ "views" match {
        case JArray(l) => l.map(viewJson =>
          viewJson.values match{
            case vals: Map[String, Any] => CompleteViewJson(vals)
            case _ => CompleteViewJson(Map.empty)
          })
        case _ => Nil
      }
    }
  }

  def addView(bankId: String, accountId: String, viewId: String, newMetadataView: String) : Box[JValue] = {
    val json =
      ("name" -> viewId) ~
        ("description" -> "default description") ~
        ("metadata_view" -> "") ~
        ("is_public" -> false) ~
        ("which_alias_to_use" -> "public") ~
        ("hide_metadata_if_alias_used" -> true) ~
        ("allowed_actions", List(
          "can_see_transaction_this_bank_account",
          "can_see_transaction_label",
          "can_see_transaction_other_bank_account")
      )
    ObpPost(s"/$versionOfApi/banks/" + urlEncode(bankId) + "/accounts/" + urlEncode(accountId) + "/views", json)
  }

  def deleteView(bankId: String, accountId: String, viewId: String) : Boolean = {
    ObpDeleteBoolean(s"/$versionOfApi/banks/" + urlEncode(bankId) + "/accounts/" + urlEncode(accountId) + "/views/" + viewId)
  }

  def updateView(bankId: String, accountId: String, viewId: String, viewUpdateJson : JValue): Box[JValue] = {
    for {
      response <- ObpPut(s"/$versionOfApi/banks/" + bankId + "/accounts/" + accountId + "/views/" + viewId, viewUpdateJson)
    } yield response
  }

  /**
   * @return The jsons for the tags that were were successfully added
   */
  def addTags(bankId : String, accountId : String, viewId : String,
      transactionId: String, tags: List[String]) : List[TransactionTagJson] = {

    val addTagJsons = tags.map(tag => {
      ("value" -> tag)
    })

    val addTagUrl = s"/$versionOfApi/banks/" + urlEncode(bankId) + "/accounts/" + urlEncode(accountId) + "/" + urlEncode(viewId) + "/transactions/" + urlEncode(transactionId) + "/metadata/tags"

    addTagJsons.map(addTagJson => ObpPost(addTagUrl, addTagJson).flatMap(_.extractOpt[TransactionTagJson])).flatten
  }

  /**
   * @return True if the tag was deleted
   */
  def deleteTag(bankId : String, accountId : String, viewId : String,
      transactionId: String, tagId: String) : Boolean  = {
    val deleteTagUrl = s"/$versionOfApi/banks/" + urlEncode(bankId) + "/accounts/" + urlEncode(accountId) + "/" + urlEncode(viewId) + "/transactions/" +
      urlEncode(transactionId) + "/metadata/tags/" + urlEncode(tagId)
    ObpDeleteBoolean(deleteTagUrl)
  }

  /**
   * @return The json for the image if it was successfully added
   */
  def addImage(bankId : String, accountId : String, viewId : String,
      transactionId: String, imageURL: String, imageDescription: String) = {

    val json =
      ("label" -> imageDescription) ~
      ("URL" -> imageURL)

    val addImageUrl = s"/$versionOfApi/banks/" + urlEncode(bankId) + "/accounts/" + urlEncode(accountId) + "/" + urlEncode(viewId) +
      "/transactions/" + urlEncode(transactionId) + "/metadata/images"

    ObpPost(addImageUrl, json).flatMap(_.extractOpt[TransactionImageJson])
  }

  /**
   * @return True if the image was deleted
   */
  def deleteImage(bankId : String, accountId : String, viewId : String,
      transactionId: String, imageId: String) : Boolean  = {

    val deleteImageUrl = s"/$versionOfApi/banks/" + urlEncode(bankId) + "/accounts/" + urlEncode(accountId) + "/" + urlEncode(viewId) +
      "/transactions/" + urlEncode(transactionId) + "/metadata/images/" + urlEncode(imageId)
    ObpDeleteBoolean(deleteImageUrl)
  }

}

object IdentityProviderRequest extends MdcLoggable {

  val clientId = Props.get("obp_consumer_key", "")
  val clientSecret = Props.get("obp_secret_key", "")
  val tokenEndpoint = Props.get("identity_provider_token_endpoint", "http://127.0.0.1:4444/oauth2/token")
  val jwsAlg = Props.get("oauth2.jws_alg", "ES256")
  val jwkPrivateKey = Props.get("oauth2.jwk_private_key")

  val jwk: Option[JWK] = jwkPrivateKey.map(JWK.parse(_))
  lazy val jwsSigner: Option[JWSSigner] = {
    if (jwkPrivateKey.isDefined) {
      if (jwsAlg.startsWith("ES")) Some(new ECDSASigner(jwk.get.asInstanceOf[ECKey]))
      else {
        if (jwsAlg.startsWith("PS")) Security.addProvider(BouncyCastleProviderSingleton.getInstance)
        val privateKey = jwk.get.asInstanceOf[RSAKey]
        Some(new RSASSASigner(privateKey))
      }
    } else {
      None
    }
  }


  private def signClaims(claimsSet: JWTClaimsSet) = {
    val jwt = new SignedJWT(new JWSHeader.Builder(JWSAlgorithm.parse(jwsAlg)).keyID(jwk.get.getKeyID).build, claimsSet)
    // Sign with private EC key
    try jwt.sign(jwsSigner.get)
    catch {
      case e: JOSEException =>
        println(e)
    }
    jwt.serialize
  }

  def buildClientAssertion: String = { // JWT claims
    //iss: REQUIRED. Issuer. This MUST contain the client_id of the OAuth Client.
    //sub: REQUIRED. Subject. This MUST contain the client_id of the OAuth Client.
    //aud: REQUIRED. Audience. The aud (audience) Claim. Value that identifies the Authorization Server (ORY Hydra) as an intended audience. The Authorization Server MUST verify that it is an intended audience for the token. The Audience SHOULD be the URL of the Authorization Server's Token Endpoint.
    //jti: REQUIRED. JWT ID. A unique identifier for the token, which can be used to prevent reuse of the token. These tokens MUST only be used once, unless conditions for reuse were negotiated between the parties; any such negotiation is beyond the scope of this specification.
    //exp: REQUIRED. Expiration time on or after which the ID Token MUST NOT be accepted for processing.
    //iat: OPTIONAL. Time at which the JWT was issued.
    val claimsSet = new JWTClaimsSet.Builder()
      .issuer(clientId).subject(clientId).audience(tokenEndpoint)
      .jwtID(UUID.randomUUID.toString)
      .expirationTime(new Date(new Date().getTime + 60 * 1000))
      .issueTime(new Date).build
    signClaims(claimsSet)
  }

  def isPublicClient() = jwkPrivateKey.isDefined

  def obtainAccessToken() = {
    import okhttp3.{OkHttpClient, Request, RequestBody}
    val client = new OkHttpClient().newBuilder.build
    val mediaType = MediaType.parse("application/x-www-form-urlencoded;charset=UTF-8")
    val grantType = "client_credentials"


    val body = isPublicClient match {
      case true =>
        RequestBody.create(mediaType, s"grant_type=$grantType&client_assertion_type=urn%3Aietf%3Aparams%3Aoauth%3Aclient-assertion-type%3Ajwt-bearer&client_assertion=$buildClientAssertion")
      case false =>
        RequestBody.create(mediaType, s"grant_type=$grantType&client_id=$clientId&client_secret=$clientSecret")
    }

    val request = new Request.Builder()
      .url(tokenEndpoint).method("POST", body)
      .addHeader("Content-Type", "application/x-www-form-urlencoded;charset=UTF-8").build
    val response = client.newCall(request).execute
    val responseBody = response.body.string
    response.close()
    responseBody
  }

}


case class ObpError(error :String)

object OBPRequest extends MdcLoggable {
  implicit val formats = DefaultFormats
  //returns a tuple of the status code and response body as a string
  def apply(apiPath : String, jsonBody : Option[JValue], method : String, headers : List[Header]) : Box[(Int, String)] = {
    val statusAndBody = tryo {
      val credentials = OAuthClient.getAuthorizedCredential
      val apiUrl = OAuthClient.currentApiBaseUrl
      val url = new URL(apiUrl + apiPath)
      logger.info(s"OBP Server Request URL: ${url.getHost}${url.getPath}")

      def addAppAccessIfNecessary: List[Header] = {
        if (!OAuthClient.loggedIn) {
          Header("Authorization", s"Bearer $obtainAccessToken") :: headers
        } else {
          headers
        }
      }

      val request = SSLHelper.getConnection(apiUrl + apiPath)
      request.setDoOutput(true)
      request.setRequestMethod(method)
      request.setRequestProperty("Content-Type", "application/json; charset=UTF-8")
      request.setRequestProperty("Accept", "application/json")
      request.setRequestProperty("Accept-Charset", "UTF-8")

      addAppAccessIfNecessary.foreach(header => request.setRequestProperty(header.key, header.value))

      //sign the request if we have some credentials to sign it with
      credentials.foreach(c => c.consumer.sign(request))

      //Set the request body
      if(jsonBody.isDefined) {
        val output = request.getOutputStream()
        val writer = new BufferedWriter(new OutputStreamWriter(output, "UTF-8"))
        writer.write(compactRender(jsonBody.get))
        writer.flush()
        writer.close()
      }

      request.connect()
      val status = request.getResponseCode()

      //get reponse body
      val inputStream = if(status >= 400) request.getErrorStream() else request.getInputStream()
      val reader = new BufferedReader(new InputStreamReader(inputStream, "UTF-8"))
      val builder = new StringBuilder()
      var line = ""
      def readLines() {
        line = reader.readLine()
        if (line != null) {
          builder.append(line + "\n")
          readLines()
        }
      }
      readLines()
      reader.close();
      (status, builder.toString())
    }

    statusAndBody pass {
      case Failure(msg, ex, _) => {
        val sw = new StringWriter()
        val writer = new PrintWriter(sw)
        ex.foreach(_.printStackTrace(writer))
        logger.debug("Error making api call: " + msg + ", stack trace: " + sw.toString)
      }
      case _ => Unit
    }
  }
  private def obtainAccessToken: String = {
    val jsonResponse = IdentityProviderRequest.obtainAccessToken()
    import net.liftweb.json._
    val jsonAst = parse(jsonResponse) \ "access_token"
    val accessToken = compactRender(jsonAst).stripPrefix("\"").stripSuffix("\"")
    accessToken
  }

}

//Ugly duplicate of above to be able to get rid of /obp prefix.
//Should be done without it
object OBPInternalRequest extends MdcLoggable {
  implicit val formats = DefaultFormats
  //returns a tuple of the status code and response body as a string
  def apply(apiPath : String, jsonBody : Option[JValue], method : String, headers : List[Header]) : Box[(Int, String)] = {
    val statusAndBody = tryo {
      val credentials = OAuthClient.getAuthorizedCredential
      val apiUrl = OBPDemo.baseUrl
      val url = new URL(apiUrl + apiPath)
      //bleh
      val request = url.openConnection().asInstanceOf[HttpURLConnection] //blagh!
      request.setDoOutput(true)
      request.setRequestMethod(method)
      request.setRequestProperty("Content-Type", "application/json")
      request.setRequestProperty("Accept", "application/json")

      headers.foreach(header => request.setRequestProperty(header.key, header.value))

      //sign the request if we have some credentials to sign it with
      credentials.foreach(c => c.consumer.sign(request))

      //Set the request body
      if(jsonBody.isDefined) {
        val output = request.getOutputStream()
        val body = compactRender(jsonBody.get).getBytes()
        output.write(body)
        output.flush()
        output.close()
      }
      request.connect()

      val status = request.getResponseCode()

      //bleh
      val inputStream = if(status >= 400) request.getErrorStream() else request.getInputStream()
      val reader = new BufferedReader(new InputStreamReader(inputStream))
      val builder = new StringBuilder()
      var line = ""
      def readLines() {
        line = reader.readLine()
        if (line != null) {
          builder.append(line + "\n")
          readLines()
        }
      }
      readLines()
      reader.close();
      (status, builder.toString())
    }

    statusAndBody pass {
      case Failure(msg, ex, _) => {
        val sw = new StringWriter()
        val writer = new PrintWriter(sw)
        ex.foreach(_.printStackTrace(writer))
        logger.debug("Error making api call: " + msg + ", stack trace: " + sw.toString)
      }
      case _ => Unit
    }
  }
}

object ObpPut {
  def apply(apiPath: String, json : JValue): Box[JValue] = {
    OBPRequest(apiPath, Some(json), "PUT", Nil).flatMap {
      case(status, result) => APIUtils.getAPIResponseBody(status, result)
    }
  }
}

object ObpPost {
  def apply(apiPath: String, json : JValue): Box[JValue] = {
    OBPRequest(apiPath, Some(json), "POST", Nil).flatMap {
      case(status, result) => APIUtils.getAPIResponseBody(status, result)
    }
  }
}

object ObpDeleteBoolean {
  /**
   * @return True if the delete worked
   */
  def apply(apiPath: String): Boolean = {
    val worked = OBPRequest(apiPath, None, "DELETE", Nil).map {
      case(status, result) => APIUtils.apiResponseWorked(status, result)
    }
    worked.getOrElse(false)
  }
}



// In case we want a more raw result
// TODO
object ObpDelete {
  def apply(apiPath: String): Box[JValue] = {
    OBPRequest(apiPath, None, "DELETE", Nil).map {
      case(status, result) => APIUtils.apiResponseWorked(status, result)
    }
  }
}




object ObpGet {
  def apply(apiPath: String, headers : List[Header] = Nil): Box[JValue] = {
    OBPRequest(apiPath, None, "GET", headers).flatMap {
      case(status, result) => APIUtils.getAPIResponseBody(status, result)
    }
  }
}

object ObpInternalDelete {
  /**
   * @return True if the delete worked
   */
  def apply(apiPath: String): Boolean = {
    val worked = OBPInternalRequest(apiPath, None, "DELETE", Nil).map {
      case(status, result) => APIUtils.apiResponseWorked(status, result)
    }
    worked.getOrElse(false)
  }
}

object APIUtils extends MdcLoggable {
  implicit val formats = DefaultFormats

  def getAPIResponseBody(responseCode : Int, body : String) : Box[JValue] = {
    responseCode match {
      case 200 | 201 => tryo{parse(body)}
      case _ => {
        val failMsg = "Bad response code (" + responseCode + ") from OBP API server: " + body
        logger.warn(failMsg)
        Failure(body)
      }
    }
  }

  def apiResponseWorked(responseCode : Int, result : String) : Boolean = {
    responseCode match {
      case 200 | 201 | 204 => true
      case _ => false
    }
  }
}

object ObpJson {
  import net.liftweb.json._
  implicit val formats = DefaultFormats
  case class BanksJson(banks : Option[List[BankJson]]) {
    def bankJsons: List[BankJson] = {
      banks.toList.flatten
    }
  }
  case class BankRoutingJsonV121(
    scheme: Option[String],
    address: Option[String]
  )
  case class BankJson(id : Option[String],
    short_name : Option[String],
    full_name : Option[String],
    logo : Option[String],
    website : Option[String],
    bank_routing: Option[BankRoutingJsonV121])

  case class BankAttributeBankResponseJsonV400(name: String,
                                               value: String)
  case class BankJson400(
                          id: String,
                          short_name: String,
                          full_name: String,
                          logo: String,
                          website: String,
                          bank_routings: List[BankRoutingJsonV121],
                          attributes: Option[List[BankAttributeBankResponseJsonV400]]
                        )

  case class BanksJson400(banks: List[BankJson400])

  case class BankJsonV400(id : Option[String],
    short_name : Option[String],
    full_name : Option[String],
    logo : Option[String],
    website : Option[String],
    bank_routing: Option[BankRoutingJsonV121])

  case class UserJSONV121(id: Option[String],
    provider: Option[String],
    display_name: Option[String])

  case class AccountBalanceJson(currency: Option[String],
    amount: Option[String])

  case class AmountOfMoneyJsonV121(
                                    currency: String,
                                    amount: String
                                  )

  case class AccountRoutingJsonV121(
                                     scheme: String,
                                     address: String
                                   )

  case class CreateAccountRequestJsonV310(
                                           user_id: String,
                                           label: String,
                                           product_code: String,
                                           balance: AmountOfMoneyJsonV121,
                                           branch_id: String,
                                           account_routings: List[AccountRoutingJsonV121]
                                         )

  case class HistoricalTransactionAccountJsonV310(
                                                   bank_id: Option[String],
                                                   account_id : Option[String],
                                                   counterparty_id : Option[String],
                                                 )
  case class PostHistoricalTransactionJson(
                                            from: HistoricalTransactionAccountJsonV310,
                                            to: HistoricalTransactionAccountJsonV310,
                                            value: AmountOfMoneyJsonV121,
                                            description: String,
                                            posted: String,
                                            completed: String,
                                            `type`: String,
                                            charge_policy: String
                                          )
  case class PostHistoricalTransactionAtBankJson(
                                                  from_account_id: String,
                                                  to_account_id: String,
                                                  value: AmountOfMoneyJsonV121,
                                                  description: String,
                                                  posted: String,
                                                  completed: String,
                                                  `type`: String,
                                                  charge_policy: String
                                                )
  case class PostHistoricalTransactionResponseJson(
                                                    transaction_id: String,
                                                    from: HistoricalTransactionAccountJsonV310,
                                                    to: HistoricalTransactionAccountJsonV310,
                                                    value: AmountOfMoneyJsonV121,
                                                    description: String,
                                                    posted: Date,
                                                    completed: Date,
                                                    transaction_request_type: String,
                                                    charge_policy: String
                                                  )
  case class TransactionAttributeJsonV400(
                                           name: String,
                                           `type`: String,
                                           value: String,
                                         )
  case class TransactionAttributeResponseJson(
                                               transaction_attribute_id: String,
                                               name: String,
                                               `type`: String,
                                               value: String
                                             )
    //simplified version of what we actually get back from the api
  case class ViewJson(
    id: Option[String],
    short_name: Option[String],
    description: Option[String],
    is_public: Option[Boolean])

  case class ViewsJson(views: Option[List[ViewJson]])

  case class CompleteViewJson(json: Map[String, Any]){
    val id: Option[String] = json.get("id") match {
      case Some(s : String) => Some(s)
      case _ => None
    }

    val shortName: Option[String] = json.get("short_name") match {
      case Some(s : String) => Some(s)
      case _ => None
    }

    val alias: Option[String] = json.get("alias") match {
      case Some(s : String) => Some(s)
      case _ => None
    }

    val description: Option[String] = json.get("description") match {
      case Some(s : String) => Some(s)
      case _ => None
    }

    val metadataView: Option[String] = json.get("metadata_view") match {
      case Some(s : String) => Some(s)
      case _ => None
    }

    val isPublic: Option[Boolean] = json.get("is_public") match {
      case Some(b : Boolean) => Some(b)
      case _ => None
    }

    val booleans = json.collect{ case (s: String, b: Boolean) => (s,b)}

    val permissions = booleans.filterNot(_.key == "is_public")
  }

  case class AccountJson(id: Option[String],
    label: Option[String],
    number: Option[String],
    owners: Option[List[UserJSONV121]],
    `type`: Option[String],
    balance: Option[AccountBalanceJson],
    IBAN : Option[String],
    views_available: Option[List[ViewJson]])

  case class BarebonesAccountsJson(accounts: Option[List[BarebonesAccountJson]])

  case class BarebonesAccountJson(id: Option[String],
                                  label: Option[String],
                                  views: Option[List[ViewJson]],
                                  bank_id: Option[String])

  case class AccountsBalancesJsonV400(accounts:List[AccountBalanceJsonV400])

  case class BalanceJsonV400(`type`: String, currency: String, amount: String)

  case class AccountBalanceJsonV400(
                                     account_id: String,
                                     bank_id: String,
                                     account_routings: List[AccountRouting],
                                     label: String,
                                     balances: List[BalanceJsonV400]
                                   )
  case class AccountRouting(
                             scheme: String,
                             address: String
                           )


  case class HolderJson(name: Option[String],
		is_alias : Option[Boolean])

  //TODO: Can this go with BankJson?
  case class LightBankJson(national_identifier: Option[String],
    name: Option[String])

  case class ThisAccountJson(holders: Option[List[HolderJson]],
    number: Option[String],
    kind: Option[String],
    IBAN: Option[String],
    bank: Option[LightBankJson])

  case class LocationJson(latitude: Option[Double],
    longitude: Option[Double],
    date: Option[Date], //TODO: Check if the default date formatter is okay
    user: Option[UserJSONV121])

  case class OtherAccountMetadataJson(public_alias: Option[String],
    private_alias: Option[String],
    more_info: Option[String],
    URL: Option[String],
    image_URL: Option[String],
    open_corporates_URL: Option[String],
    corporate_location: Option[LocationJson],
    physical_location: Option[LocationJson])

  //TODO: Why can't an other account have more than one holder?
  case class OtherAccountJson(id: Option[String],
    holder: Option[HolderJson],
    number: Option[String],
    kind: Option[String],
    IBAN: Option[String],
    bank: Option[LightBankJson],
    metadata: Option[OtherAccountMetadataJson])

  case class OtherAccountsJson(other_accounts: Option[List[OtherAccountJson]])

  //////////////////////////////////////
  // Subtle differences to the OtherAccount json above.
  // This what the 1.2.1 other_accounts call returns
  // These case classes copied from API JSONFactory1.2.1

  case class OtherAccountJson121(
                               id : String,
                               holder : AccountHolderJson121,
                               number : String,
                               kind : String,
                               IBAN : String,
                               swift_bic: String,
                               bank : DirectMinimalBankJSON,
                               metadata : DirectOtherAccountMetadataJSON
                               )

  case class DirectOtherAccountsJson(
                                other_accounts : List[OtherAccountJson121]
                                )

  case class AccountHolderJson121(
    name : String,
    is_alias : Boolean
    )

  case class DoubleEntryTransactionJson(
                                         transaction_request: TransactionRequestBankAccountJson,
                                         debit_transaction: TransactionBankAccountJson,
                                         credit_transaction: TransactionBankAccountJson
                                       )

  case class TransactionRequestBankAccountJson(
                                                bank_id: String,
                                                account_id: String,
                                                transaction_request_id: String
                                              )

  case class TransactionBankAccountJson(
                                         bank_id: String,
                                         account_id: String,
                                         transaction_id: String
                                       )

  case class DirectMinimalBankJSON(
  national_identifier : String,
  name : String
  )

  case class DirectOtherAccountMetadataJSON(
   public_alias : String,
   private_alias : String,
   more_info : String,
   URL : String,
   image_URL : String,
   open_corporates_URL : String,
   corporate_location : DirectLocationJSON,
   physical_location : DirectLocationJSON
   )


  case class DirectLocationJSON(
  latitude : Double,
  longitude : Double,
  date : Date,
  user : DirectUserJSON
  )


  case class DirectUserJSON(
   id : String,
   provider : String,
   display_name : String
   )


  ///////////

  case class TransactionValueJson(currency: Option[String],
    amount: Option[String])

  case class TransactionDetailsJson(`type`: Option[String],
    description: Option[String],
    posted: Option[Date], //TODO: Check if the default date formatter is okay
    completed: Option[Date], //TODO: Check if the default date formatter is okay
    new_balance: Option[AccountBalanceJson],
    value: Option[TransactionValueJson])

  case class TransactionCommentJson(id: Option[String],
    date: Option[Date], //TODO: Check if the default date formatter is okay
    value: Option[String],
    user: Option[UserJSONV121],
    reply_to: Option[String])

  case class TransactionTagJson(id: Option[String],
    date: Option[Date], //TODO: Check if the default date formatter is okay
    value: Option[String],
    user: Option[UserJSONV121])

  case class TransactionImageJson(id: Option[String],
    label: Option[String],
    date: Option[Date], //TODO: Check if the default date formatter is okay
    URL: Option[String],
    user: Option[UserJSONV121])

  case class TransactionMetadataJson(narrative: Option[String],
    comments: Option[List[TransactionCommentJson]],
    tags: Option[List[TransactionTagJson]],
    images: Option[List[TransactionImageJson]],
    where: Option[LocationJson])

  case class TransactionJson(uuid: Option[String],
    id: Option[String],
    this_account: Option[ThisAccountJson],
    other_account: Option[OtherAccountJson],
    details: Option[TransactionDetailsJson],
    metadata: Option[TransactionMetadataJson]) {

    lazy val imageJsons : Option[List[TransactionImageJson]] = {
      metadata.flatMap(_.images)
    }

    lazy val tagJsons : Option[List[TransactionTagJson]] = {
      metadata.flatMap(_.tags)
    }

    lazy val commentJsons : Option[List[TransactionCommentJson]] = {
      metadata.flatMap(_.comments)
    }
  }

  case class TransactionsJson(transactions: Option[List[TransactionJson]])

  case class PermissionJson(user: Option[UserJSONV121], views: Option[List[ViewJson]])

  case class PermissionsJson(permissions : Option[List[PermissionJson]])


  // Copied directly from 1.2.1 API
  case class TransactionsJson121(
   transactions: List[TransactionJson121]
   )

  case class TransactionJson121(
      id : String,
      this_account : ThisAccountJson121,
      other_account : OtherAccountJson121,
      details : TransactionDetailsJson121,
      metadata : TransactionMetadataJson121
      )

  case class ThisAccountJson121(
    id : String,
    holders : List[AccountHolderJson121],
    number : String,
    kind : String,
    IBAN : String,
    swift_bic: String,
    bank : MinimalBankJson121
    )


  case class MinimalBankJson121(
    national_identifier : String,
    name : String
    )


  case class TransactionDetailsJson121(
   `type` : String,
   description : String,
   posted : Date,
   completed : Date,
   new_balance : AmountOfMoneyJson121,
   value : AmountOfMoneyJson121
 )

  case class TransactionMetadataJson121(
  narrative : String,
  comments : List[TransactionCommentJson121],
  tags :  List[TransactionTagJson121],
  images :  List[TransactionImageJson121],
  where : LocationJson121
  )

  case class LocationJson121(
                           latitude : Double,
                           longitude : Double,
                           date : Date,
                           user : UserJson121
                           )

  case class AmountOfMoneyJson121(
    currency : String,
    amount : String
    )


  case class TransactionCommentJson121(
     id : String,
     value : String,
     date: Date,
     user : UserJson121
     )



  case class TransactionTagJson121(
   id : String,
   value : String,
   date : Date,
   user : UserJson121
   )


  case class TransactionImageJson121(
   id : String,
   label : String,
   URL : String,
   date : Date,
   user : UserJson121
   )

  case class UserJson121(
   id : String,
   provider : String,
   display_name : String
   )



  ////////////////////////////////////////
  // Copied from OBP-API JSONFactory1_4_0
  // TODO: Import these and others from API jar file?

  // Matches OBP-API representation of Resource Docs etc. Used to describe where an API call is implemented
  case class ImplementedByJson (
                                 version : String, // Short hand for the version e.g. "1_4_0" means Implementations1_4_0
                                 function : String // The val / partial function that implements the call e.g. "getBranches"
                                 )


  // Used to describe the OBP API calls for documentation and API discovery purposes
  case class ResourceDocJson(operation_id: String,
                             request_verb: String,
                             request_url: String,
                             summary: String, // Summary of call should be 120 characters max
                             description: String,      // Description of call in markdown
                             example_request_body: JValue,  // An example request body
                             success_response_body: JValue, // Success response body
                             implemented_by: ImplementedByJson)

  case class ResourceDocsJson (resource_docs : List[ResourceDocJson])
  ///////////////////////////////////////////


  // Internal representation of the ResourceDoc (may differ from the OBP API representation (for instance OBP representation does not have id)
  case class ResourceDoc(id: String,
                         verb: String,
                         url: String,
                         summary: String,
                         description: NodeSeq,
                         example_request_body: JValue)


  case class ResourceDocs (resourceDocs : List[ResourceDoc])

  case class CurrentUserJson(user_id: String,
                             email: String,
                             provider_id: String,
                             provider: String,
                             username: String
                            )

  case class UserCustomerLinkJson(
                                   user_customer_link_id: String,
                                   customer_id: String,
                                   user_id: String,
                                   date_inserted: Date,
                                   is_active: Boolean
                                 )
  case class UserCustomerLinksJson(user_customer_links: List[UserCustomerLinkJson])

  case class CreateUserCustomerLinkJson(user_id: String, customer_id: String)

  case class PostCustomerJsonV310(
                                   legal_name: String,
                                   mobile_phone_number: String,
                                   email: String,
                                   face_image: CustomerFaceImageJson,
                                   date_of_birth: Date,
                                   relationship_status: String,
                                   dependants: Int,
                                   dob_of_dependants: List[Date],
                                   credit_rating: CustomerCreditRatingJSON,
                                   credit_limit: AmountOfMoneyJsonV121,
                                   highest_education_attained: String,
                                   employment_status: String,
                                   kyc_status: Boolean,
                                   last_ok_date: Date,
                                   title: String,
                                   branch_id: String,
                                   name_suffix: String
                                 )
  case class CustomerFaceImageJson(url : String, date : Date)
  case class CustomerCreditRatingJSON(rating: String, source: String)

  case class CustomerJsonV310(
                               bank_id: String,
                               customer_id: String,
                               customer_number : String,
                               legal_name : String,
                               mobile_phone_number : String,
                               email : String,
                               face_image : CustomerFaceImageJson,
                               date_of_birth: Date,
                               relationship_status: String,
                               dependants: Integer,
                               dob_of_dependants: List[Date],
                               credit_rating: Option[CustomerCreditRatingJSON],
                               credit_limit: Option[AmountOfMoneyJsonV121],
                               highest_education_attained: String,
                               employment_status: String,
                               kyc_status: lang.Boolean,
                               last_ok_date: Date,
                               title: String,
                               branch_id: String,
                               name_suffix: String
                             )

  case class GetUserJsonV400(
    user_id: String,
    email: String,
    provider_id:String,
    provider: String,
    username: String,
    entitlements:EntitlementJSONs)


  case class PostUserJsonV400(
                              email: String,
                              username: String,
                              password: String,
                              first_name: String,
                              last_name: String)





  trait Entitlement {
    def entitlementId: String
    def bankId : String
    def userId : String
    def roleName : String
    def createdByProcess : String
  }

  //case class CreateEntitlementJSON(bank_id: String, role_name: String)
  case class EntitlementJSON(entitlement_id: String, role_name: String, bank_id: String)
  case class EntitlementJSONs(list: List[EntitlementJSON])


  def createEntitlementJSON(e: Entitlement): EntitlementJSON = {
    EntitlementJSON(entitlement_id = e.entitlementId,
      role_name = e.roleName,
      bank_id = e.bankId)
  }

  def createEntitlementJSONs(l: List[Entitlement]): EntitlementJSONs = {
    EntitlementJSONs(l.map(createEntitlementJSON))
  }


  case class CustomerWithAttributesJsonV300(
                                             bank_id: String,
                                             customer_id: String,
                                             customer_number : String,
                                             legal_name : String,
                                             mobile_phone_number : String,
                                             email : String,
                                             face_image : CustomerFaceImageJson,
                                             date_of_birth: String,
                                             relationship_status: String,
                                             dependants: Integer,
                                             dob_of_dependants: List[String],
                                             credit_rating: Option[CustomerCreditRatingJSON],
                                             credit_limit: Option[AmountOfMoneyJsonV121],
                                             highest_education_attained: String,
                                             employment_status: String,
                                             kyc_status: lang.Boolean,
                                             last_ok_date: Date,
                                             title: String,
                                             branch_id: String,
                                             name_suffix: String,
                                             customer_attributes: List[CustomerAttributeResponseJsonV300]
                                           )
  case class CustomerAttributeResponseJsonV300(
                                                customer_attribute_id: String,
                                                name: String,
                                                `type`: String,
                                                value: String
                                              )

  case class CustomersWithAttributesJsonV300(customers: List[CustomerWithAttributesJsonV300])

  case class UserAttributeResponseJsonV400(
    user_attribute_id: String,
    name: String,
    `type`: String,
    value: String,
    insert_date: Date
  )

  case class UserAttributesResponseJson(
                                         user_attributes: List[UserAttributeResponseJsonV400]
                                       )

  case class UserAttributeJsonV400(
                                    name: String,
                                    `type`: String,
                                    value: String,
                                  )

  case class UserWithAttributesResponseJson(
    user_id: String,
    email : String,
    provider_id: String,
    provider : String,
    username : String,
    user_attributes: List[UserAttributeResponseJsonV400]
  )
  case class CustomerAndUsersWithAttributesResponseJson(
    customer: CustomerJsonV310,
    users: List[UserWithAttributesResponseJson]
  )

  case class CorrelatedEntities(
    correlated_entities: List[CustomerAndUsersWithAttributesResponseJson]
  )

}
