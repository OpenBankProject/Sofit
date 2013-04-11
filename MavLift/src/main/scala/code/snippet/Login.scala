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
    ".logout [onclick]" #> SHtml.onEvent(s => {
      OAuthClient.logoutAll()
      S.redirectTo("/")
      Noop
    })
  }
  
  def loggedOut = {
    
    val provider = OAuthClient.defaultProvider
    
    ".logged-in *" #> "" &
    ".start-login [onclick]" #> SHtml.onEvent(s => {
      val bankAuthUrl = OAuthClient.getAuthUrl(provider)
      val guestAuthUrl = bankAuthUrl
      JsRaw("jQuery('.provider-name').text('" + provider.name + "')").cmd &
      JsRaw("jQuery('.bank-login').attr('href', '" + bankAuthUrl + "')").cmd &
      JsRaw("jQuery('.guest-login').attr('href', '" + guestAuthUrl + "')").cmd &
      Show("choose-login-link")
    })
  }
  
  def login = {
    println("logged in at any:" + OAuthClient.loggedInAtAny)
    if(OAuthClient.loggedInAtAny) loggedIn
    else loggedOut
  }

}