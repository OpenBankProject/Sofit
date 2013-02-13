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
package code.api

import code.actors.EnvelopeInserter
import net.liftweb.http._
import net.liftweb.http.rest._
import net.liftweb.json.JsonDSL._
import net.liftweb.json.Printer._
import net.liftweb.json.Extraction._
import net.liftweb.json.JsonAST._
import net.liftweb.common.{Failure,Full,Empty}
import net.liftweb.mongodb._
import net.liftweb.json.JsonAST.JString
import com.mongodb.casbah.Imports._
import _root_.java.math.MathContext
import org.bson.types._
import org.joda.time.{ DateTime, DateTimeZone }
import java.util.regex.Pattern
import _root_.net.liftweb.util._
import _root_.net.liftweb.mapper._
import _root_.net.liftweb.util.Helpers._
import _root_.net.liftweb.sitemap._
import _root_.scala.xml._
import _root_.net.liftweb.http.S._
import _root_.net.liftweb.http.RequestVar
import _root_.net.liftweb.util.Helpers._
import _root_.net.liftweb.common.Full
import net.liftweb.mongodb.{ Skip, Limit }
import _root_.net.liftweb.http.S._
import _root_.net.liftweb.mapper.view._
import com.mongodb._
import code.model.dataAccess.LocalStorage
import code.model.traits.ModeratedTransaction
import code.model.traits.View
import code.model.implementedTraits.View
import code.model.traits.BankAccount
import code.model.implementedTraits.Public
import code.model.traits.Bank
import code.model.traits.User
import java.util.Date
import code.api.OAuthHandshake._
import code.model.traits.ModeratedBankAccount

object OBPAPI1_1 extends RestHelper with Loggable {

  val dateFormat = ModeratedTransaction.dateFormat
  private def getUser(httpCode : Int, tokenID : Box[String]) : Box[User] = 
  if(httpCode==200)
  {
    import code.model.Token
    Token.find(By(Token.key, tokenID.get)) match {
      case Full(token) => User.findById(token.userId)
      case _ => Empty   
    }         
  }
  else 
    Empty 
  
  serve("obp" / "v1.1" prefix {
    
    case Nil JsonGet json => {
      
      def gitCommit : String = {
        val commit = tryo{
          val properties = new java.util.Properties()
          properties.load(getClass().getClassLoader().getResourceAsStream("git.properties"))
          properties.getProperty("git.commit.id", "")
        }
        commit getOrElse ""
      }
      
      val apiDetails = {
        ("api" ->
          ("version" -> "1.1") ~
          ("git_commit" -> gitCommit) ~
          ("hosted_by" -> 
            ("organisation" -> "TESOBE") ~
            ("email" -> "contact@tesobe.com") ~
            ("phone" -> "+49 (0)30 8145 3994"))) ~
        ("links" -> 
          ("rel" -> "banks") ~
          ("href" -> "/banks") ~
          ("method" -> "GET") ~
          ("title" -> "Returns a list of banks supported on this server"))
      }
      
      JsonResponse(apiDetails)
    }
    
    case "banks" :: Nil JsonGet json => {
      def bankToJson( b : Bank) = {
        ("bank" -> 
          ("id" -> b.permalink) ~
          ("short_name" -> b.shortName) ~
          ("full_name" -> b.fullName) ~
          ("logo" -> b.logoURL) 
        )
      }

      JsonResponse("banks" -> Bank.all.map(bankToJson _ ))
    }  
  })
}