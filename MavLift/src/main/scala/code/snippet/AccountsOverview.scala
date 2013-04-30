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

import net.liftweb.util.Helpers._
import code.model.traits.BankAccount
import net.liftweb.common.Full
import code.model.dataAccess.OBPUser
import code.model.traits.User
import code.model.implementedTraits.Public
import code.model.implementedTraits.Owner
import code.model.implementedTraits.Board
import code.model.implementedTraits.Authorities
import code.model.implementedTraits.Team
import code.model.implementedTraits.OurNetwork
import net.liftweb.http.SHtml
import scala.xml.Text
import net.liftweb.http.js.JsCmds.Noop

class AccountsOverview {

  def publicAccounts = {
    //TODO: In the future once we get more bank accounts we will probably want some sort of pagination or limit on the number of accounts displayed
    val publicAccounts = BankAccount.publicAccounts.sortBy(acc => acc.label)
    if(publicAccounts.size == 0)
      ".accountList" #> "No public accounts"
    else
      ".accountList" #> publicAccounts.map(acc => {
        ".accLink *" #> acc.label &
        //TODO: Would be nice to be able to calculate this is in a way that would be less fragile in terms of maintenance
        ".accLink [href]" #> { "/banks/" + acc.bankPermalink + "/accounts/" + acc.permalink + "/" + Public.permalink }
      })
  }

  def authorisedAccounts = {
    def loggedInSnippet(user: User) = {
      val accountsWithMoreThanAnonAccess = user.accountsWithMoreThanAnonAccess.toList.sortBy(acc => acc.label)
      ".accountList" #> accountsWithMoreThanAnonAccess.map(acc => {
        ".accLink *" #> acc.name &
        //TODO: Would be nice to be able to calculate this is in a way that would be less fragile in terms of maintenance
        ".accLink [href]" #> {
          val permittedViews = user.permittedViews(acc)
          val highestViewPermalink = {
            //Make sure that the link always points to the same view by giving some order instead of picking the first one
            if(permittedViews.contains(Owner)) Owner.permalink
            else if (permittedViews.contains(Board)) Board.permalink
            else if (permittedViews.contains(Authorities)) Authorities.permalink
            else if (permittedViews.contains(Team)) Team.permalink
            else OurNetwork.permalink
           }
          "/banks/" + acc.bankPermalink + "/accounts/" + acc.permalink + "/" + highestViewPermalink
        }
      })
    }

    def loggedOutSnippet = {
      ".accountList" #> SHtml.span(Text("You don't have access to any authorised account"), Noop,("id","accountsMsg"))
    }

    OBPUser.currentUser match {
      case Full(u) => loggedInSnippet(u)
      case _ => loggedOutSnippet
    }
  }

}