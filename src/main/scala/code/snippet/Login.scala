/**
Open Bank Project - Sofi Web Application
Copyright (C) 2011 - 2021, TESOBE GmbH

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
TESOBE GmbH.
Osloer Str. 16/17
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

import code.Constant
import code.lib.{OAuthClient, ObpAPI}
import code.util.{Helper, Util}
import net.liftweb.common.{Box, Full}
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds.Noop
import net.liftweb.http.provider.HTTPCookie
import net.liftweb.http.{S, SHtml}
import net.liftweb.util.Helpers._

class Login {
  
  
  private def loggedIn = {
    // Correlated User ID Flow
    S.cookieValue(Constant.correlatedUserIdCookieName) match {
      case Full(correlatedUserId) if correlatedUserId != null => {
        Util.correlatedUserFlow(correlatedUserId)
        S.deleteCookie(Constant.correlatedUserIdCookieName)
        S.addCookie(HTTPCookie(Constant.correlatedUserIdBoundCookieName, correlatedUserId))
      }
      case _ => 
    }
    def getDisplayNameOfUser(): Box[String] = {
      ObpAPI.currentUser.map {
        u =>
          u.provider.toLowerCase() match {
            case provider if provider.contains("google") => u.email
            case provider if provider.contains("yahoo")  => u.email
            case _                                       => u.username
          }
      }
    }
    ".logged-out *" #> "" &
    "#logged-in-name *" #> getDisplayNameOfUser() &
    "#logout [onclick+]" #> SHtml.onEvent(s => {
      OAuthClient.logoutAll()
      Noop
    })
  }

  def loggedOut = {
    ".logged-in *" #> "" &
    "#start-login [onclick]" #> {
      def actionJS: JsCmd = {
        OAuthClient.redirectToOauthLogin()
        Noop
      }
      SHtml.onEvent((s: String) => actionJS)
    }
  }

  def login = {
    if(OAuthClient.loggedIn) loggedIn
    else loggedOut
  }




}
