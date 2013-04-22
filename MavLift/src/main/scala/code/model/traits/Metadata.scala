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

package code.model.traits
import java.util.Date
import java.net.URL

trait TransactionMetadata {

  // Owner provided comment, done in OBP
  def ownerComment : Option[String]
  def ownerComment(comment : String) : Unit
  def comments : List[Comment]
  def addComment(userId : String, viewId : Long, text : String, postedDate : Date) : Unit
  def tags : List[Tag]
  def addTag(userId : String, viewId : Long, tag : String, postedDate :Date) : String
  def deleteTag(id : String) : Unit
  def images : List[TransactionImage]
  def addImage(userId: String, viewId : Long, description: String, datePosted : Date, imageUrl : URL) : String
  def deleteImage(id : String) : Unit
  def addWhereTag(userId: String, viewId : Long, datePosted : Date, longitude : Double, latitude : Double) : Boolean
  def whereTags : List[GeoTag]
}

trait OtherBankAccountMetadata
{
	def publicAlias : String
  def privateAlias : String
  def moreInfo : String
	def url : String
	def imageUrl : String
	def openCorporatesUrl : String
  def corporateLocations : List[GeoTag]
  def physicalLocations : List[GeoTag]
  def addMoreInfo(moreInfo : String ) : Boolean
  def addURL(url : String) : Boolean
  def addCorporateLocation(userId: String, viewId : Long, datePosted : Date, longitude : Double, latitude : Double) : Boolean
  def addPhysicalLocation(userId: String, viewId : Long, datePosted : Date, longitude : Double, latitude : Double) : Boolean
}