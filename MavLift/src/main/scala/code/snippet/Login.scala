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

package code.snippet

import code.model.dataAccess.{OBPUser,Account}
import scala.xml.NodeSeq
import net.liftweb.util.Helpers
import net.liftweb.util.Helpers._
import net.liftweb.util.CssSel
import net.liftweb.http.S
import net.liftweb.http.SHtml
import net.liftweb.http.js.jquery.JqJsCmds.Show
import net.liftweb.http.js.JE.JsRaw
import code.lib.OAuthClient
import oauth.signpost.basic.DefaultOAuthProvider
import code.lib.SofiAPITransition
import code.lib.SofiAPITransition
import oauth.signpost.basic.DefaultOAuthConsumer
import code.lib.OAuthClient
import net.liftweb.http.js.JsCmds.Noop

class Login {

  def loggedIn = {
    val providers = OAuthClient.loggedInAt
    
    ".logged-out *" #> "" &
    ".providers" #> {
      ".provider" #> providers.map(provider => {
        "* *" #> provider.name
      }) 
    } &
    ".logout [onclick+]" #> SHtml.onEvent(s => {
      OAuthClient.logoutAll()
      Noop
    })
  }
  
  def loggedOut = {
    
    ".logged-in *" #> "" &
    "#start-login [onclick+]" #> SHtml.onEvent(s => {
      val provider = OAuthClient.defaultProvider
      val authUrl = OAuthClient.getAuthUrl(provider)
      /**
       * For now this is just a link to the signup for the API (which is currently the same as the Social Finance app). 
       * Eventually it will end up as different url on the same provider's oauth server. How this will work in practice is
       * yet to be seen... the implementation details in OauthClient may get tricky. 
       */
      val guestLogin = OAuthClient.defaultProvider.signupUrl.getOrElse(authUrl)
      JsRaw("jQuery('.provider-name').text('" + provider.name + "')").cmd &
      JsRaw("jQuery('.bank-login').attr('href', '" + authUrl + "')").cmd &
      JsRaw("jQuery('.guest-login').attr('href', '" + guestLogin + "')").cmd &
      Show("choose-login-link")
    })
  }
  
  def login = {
    if(OAuthClient.loggedInAtAny) loggedIn
    else loggedOut
  }

}