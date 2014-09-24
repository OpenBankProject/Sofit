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

sealed trait Provider {
  val name : String

  val apiBaseUrl : String
  val requestTokenUrl : String
  val accessTokenUrl : String
  val authorizeUrl : String
  val signupUrl : Option[String]

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

  val baseUrl = Props.get("api_hostname", S.hostName)
  val apiBaseUrl = baseUrl + "/obp"
  val requestTokenUrl = baseUrl + "/oauth/initiate"
  val accessTokenUrl = baseUrl + "/oauth/token"
  val authorizeUrl = baseUrl + "/oauth/authorize"
  val signupUrl = Some(baseUrl + "/user_mgt/sign_up")

  val oAuthProvider : OAuthProvider = new DefaultOAuthProvider(requestTokenUrl, accessTokenUrl, authorizeUrl)

  val consumerKey = Props.get("obp_consumer_key", "")//SofiAPITransition.sofiConsumer.key.get
  val consumerSecret = Props.get("obp_secret_key", "")//SofiAPITransition.sofiConsumer.secret.get
}

case class Consumer(consumerKey : String, consumerSecret : String) {
  val oAuthConsumer : OAuthConsumer = new DefaultOAuthConsumer(consumerKey, consumerSecret)
}

case class Credential(provider : Provider, consumer : OAuthConsumer, readyToSign : Boolean)

object credentials extends SessionVar[Option[Credential]](None)
object mostRecentLoginAttemptProvider extends SessionVar[Box[Provider]](Empty)

object OAuthClient extends Loggable {

  val defaultProvider = OBPDemo

  def getAuthorizedCredential(provider : Provider) : Option[Credential] = {
    credentials.filter(_.readyToSign)
  }

  def replaceCredential(provider : Provider) : Credential = {
    val consumer = new DefaultOAuthConsumer(provider.consumerKey, provider.consumerSecret)
    val credential = Credential(provider, consumer, false)

    credentials.set(Some(credential))
    credential
  }

  def getOrCreateCredential(provider : Provider) : Credential = {
    credentials.get match {
      case Some(c) => c
      case None => replaceCredential(provider)
    }
  }

  def handleCallback(): Box[LiftResponse] = {

    val success = for {
      verifier <- S.param("oauth_verifier") ?~ "No oauth verifier found"
      provider <- mostRecentLoginAttemptProvider.get ?~ "No provider found for callback"
      consumer <- Box(credentials.map(_.consumer)) ?~ "No consumer found for callback"
    } yield {
      //after this, consumer is ready to sign requests
      provider.oAuthProvider.retrieveAccessToken(consumer, verifier)
      //update the session credentials
      val newCredential = Credential(provider, consumer, true)
      credentials.set(Some(newCredential))
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
    val credential = replaceCredential(provider)
    provider.oAuthProvider.retrieveRequestToken(credential.consumer, Props.get("hostname", S.hostName) + "/oauthcallback")
  }

  def loggedIn : Boolean = credentials.map(_.readyToSign).getOrElse(false)

  def logoutAll() = {
    credentials.set(None)
    S.redirectTo("/")
  }
}