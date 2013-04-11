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

class Login {

  def loggedIn = {
    if(!OBPUser.loggedIn_?){
      "*" #> NodeSeq.Empty
    }else{
      ".logout [href]" #> {
        OBPUser.logoutPath.foldLeft("")(_ + "/" + _)
      } &
      ".username *" #> OBPUser.currentUser.get.email.get 
    }
  }
  
  def loggedOut = {
    
    val provider = new DefaultOAuthProvider("http://127.0.0.1:8080/oauth/initiate",
    								 "http://127.0.0.1:8080/oauth/token",
    								 "http://127.0.0.1:8080/oauth/authorize")
    
    val appKey = SofiAPITransition.sofiConsumer.key.get
    val appSecret = SofiAPITransition.sofiConsumer.secret.get
    
    val consumer = new DefaultOAuthConsumer(appKey, appSecret)
    
    ".start-login [onclick]" #> SHtml.onEvent(s => {
      val bankAuthUrl = OAuthClient.getAuthUrl(OAuthClient.defaultProvider)
      val guestAuthUrl = bankAuthUrl
      
      JsRaw("jQuery('.bank-login').attr('href', '" + bankAuthUrl + "')").cmd &
      JsRaw("jQuery('.guest-login').attr('href', '" + guestAuthUrl + "')").cmd &
      Show("choose-login-link")
    })
  }

}