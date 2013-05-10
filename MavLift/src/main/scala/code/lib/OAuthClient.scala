/** 
Open Bank Project - Transparency / Social Finance Web Application
Copyright (C) 2011, 2012, TESOBE / Music Pictures Ltd

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

Email: contact@tesobe.com 
TESOBE / Music Pictures Ltd 
Osloerstrasse 16/17
Berlin 13359, Germany

  This product includes software developed at
  TESOBE (http://www.tesobe.com/)
  by 
  Simon Redfern : simon AT tesobe DOT com
  Stefan Bethge : stefan AT tesobe DOT com
  Everett Sochowski : everett AT tesobe DOT com
  Ayoub Benali: ayoub AT tesobe DOT com

 */

package code.lib

import net.liftweb.http.SessionVar
import net.liftweb.common.Box
import net.liftweb.common.Empty
import oauth.signpost.OAuthProvider
import oauth.signpost.basic.DefaultOAuthProvider
import net.liftweb.util.Props
import net.liftweb.http.S
import oauth.signpost.OAuthConsumer
import oauth.signpost.basic.DefaultOAuthConsumer
import net.liftweb.mapper.By
import net.liftweb.common.{Full, Failure}
import net.liftweb.util.Helpers
import net.liftweb.http.LiftResponse
import net.liftweb.common.Loggable
import code.model.dataAccess.OBPUser

sealed trait Provider {
  val name : String
  
  val apiBaseUrl : String
  val apiVersion : String
  val requestTokenUrl : String
  val accessTokenUrl : String
  val authorizeUrl : String
  
  /**
   * Can't do oAuthProvider = new DefaultOAuthProvider(requestTokenUrl, accessTokenUrl, authorizeUrl)
   * here as the Strings all evaluate at null at this point in object creation
   */
  val oAuthProvider : OAuthProvider
  
  val consumerKey : String
  val consumerSecret : String
}

object OBPDemo extends Provider {
  val name = "The Open Bank Project Demo"
    
  val baseUrl = Props.get("hostname", S.hostName)
  val apiBaseUrl = baseUrl + "/obp/v1.2"
  val apiVersion = "1.1"
  val requestTokenUrl = baseUrl + "/oauth/initiate"
  val accessTokenUrl = baseUrl + "/oauth/token"
  val authorizeUrl = baseUrl + "/oauth/authorize"
  
  val oAuthProvider : OAuthProvider = new DefaultOAuthProvider(requestTokenUrl, accessTokenUrl, authorizeUrl)
  
  val consumerKey = SofiAPITransition.sofiConsumer.key.get
  val consumerSecret = SofiAPITransition.sofiConsumer.secret.get
}

case class Consumer(consumerKey : String, consumerSecret : String) {
  val oAuthConsumer : OAuthConsumer = new DefaultOAuthConsumer(consumerKey, consumerSecret)
}

case class Credential(provider : Provider, consumer : OAuthConsumer, readyToSign : Boolean)

object credentials extends SessionVar[List[Credential]](Nil)
object mostRecentLoginAttemptProvider extends SessionVar[Box[Provider]](Empty)

/**
 * Until the Social Finance app and the API are fully split, the Social Finance app will in fact call
 * its own API functions which requires it be registered as a consumer. This object takes care of that.
 */
object SofiAPITransition {
  
  //At the moment developer email has to be unique for code.model.Consumers, which is probably not how it should be.
  //The end result is that we should search based on it.
  val sofiEmail = "socialfinance@tesobe.com"
  
  def getOrCreateSofiConsumer : code.model.Consumer = {
    code.model.Consumer.find(By(code.model.Consumer.developerEmail, sofiEmail)) match {
      case Full(c) => c
      case _ => {
        code.model.Consumer.create.name("Social Finance").
        	appType(code.model.AppType.Web).description("").developerEmail(sofiEmail).isActive(true).
        	key(Helpers.randomString(40).toLowerCase).secret(Helpers.randomString(40).toLowerCase).saveMe()
      }
    }
  }
  
  //Once Sofi splits from the api we won't need to logout the obpuser anymore as it won't exist
  def logoutOBPUserToo() = {
    OBPUser.logoutCurrentUser
  }
  
  val sofiConsumer = getOrCreateSofiConsumer
}

object OAuthClient extends Loggable {

  val defaultProvider = OBPDemo
  
  def getCredential(provider : Provider) : Option[Credential] = {
    credentials.find(_.provider == provider)
  }
  
  def getOrCreateCredential(provider : Provider) : Credential = {
    credentials.find(_.provider == provider) match {
      case Some(c) => c
      case None => {
        val consumer = new DefaultOAuthConsumer(provider.consumerKey, provider.consumerSecret)
        val credential = Credential(provider, consumer, false)
        credentials.set(credential :: credentials.get)
        credential
      }
    }
  }

  def handleCallback(): Box[LiftResponse] = {

    val success = for {
      verifier <- S.param("oauth_verifier") ?~ "No oauth verifier found"
      provider <- mostRecentLoginAttemptProvider.get ?~ "No provider found for callback"
      consumer <- Box(credentials.find(_.provider == provider).map(_.consumer)) ?~ "No consumer found for callback"
    } yield {
      //after this, consumer is ready to sign requests
      provider.oAuthProvider.retrieveAccessToken(consumer, verifier)
      //update the session credentials
      val newCredential = Credential(provider, consumer, true)
      val newCredentials = newCredential :: credentials.filterNot(_.provider == provider)
      credentials.set(newCredentials)
    }

    success match {
      case Full(_) => S.redirectTo("/") //TODO: Allow this redirect to be customised
      case Failure(msg, _, _) => logger.warn(msg)
      case _ => logger.warn("Something went wrong in an oauth callback and there was no error message set for it")
    }
    Empty
  }
		  						 
  def getAuthUrl(provider : Provider) : String = {
    mostRecentLoginAttemptProvider.set(Full(provider))
    
    val credential = getOrCreateCredential(provider)
    provider.oAuthProvider.retrieveRequestToken(credential.consumer, Props.get("hostname", S.hostName) + "/oauthcallback")
  }
  
  def loggedInAt : List[Provider] = {
    val loggedin = credentials.filter(_.readyToSign).map(_.provider)
    credentials.filter(_.readyToSign).map(_.provider)
  }
  def loggedInAt(provider : Provider) : Boolean = loggedInAt.contains(provider)
  def loggedInAtAny : Boolean = loggedInAt.size > 0
  def logoutAll() = {
    SofiAPITransition.logoutOBPUserToo()
    S.session.open_!.destroySessionAndContinueInNewSession(S.redirectTo("/"))
  }
}