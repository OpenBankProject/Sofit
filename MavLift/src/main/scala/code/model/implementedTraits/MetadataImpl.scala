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
package code.model.implementedTraits

import code.model.traits._
import net.liftweb.common.Loggable
import java.util.Date
import java.net.URL

class OtherBankAccountMetadataImpl(
  publicAlias_ : String,
  privateAlias_ : String,
  moreInfo_ : String,
  url_ : String,
  imageUrl_ : String,
  openCorporatesUrl_ : String,
  corporateLocations_ : List[GeoTag],
  physicalLocations_ : List[GeoTag],
  addMoreInfoFunc : (String) => Boolean,
  addUrlFunc : (String) => Boolean,
  addImageURLFunc : (String) => Boolean,
  addOpenCorporatesUrlFunc : (String) => Boolean,
  addCorporateLocationFunc : (String, Long, Date, Double, Double) => Boolean,
  addPhysicalLocationFunc : (String, Long, Date, Double, Double) => Boolean
) extends OtherBankAccountMetadata {

  def publicAlias : String = publicAlias_
  def privateAlias : String = privateAlias_
  def moreInfo : String = moreInfo_
  def url : String = url_
  def imageUrl : String = imageUrl_
  def openCorporatesUrl : String = openCorporatesUrl_
  def corporateLocations : List[GeoTag] = corporateLocations_
  def physicalLocations : List[GeoTag] = physicalLocations_
  def addMoreInfo(moreInfo : String) = addMoreInfoFunc(moreInfo)
  def addURL(url : String) : Boolean = addUrlFunc(url)
  def addImageURL(url : String) : Boolean = addImageURLFunc(url)
  def addOpenCorporatesUrl(url : String) : Boolean = addOpenCorporatesUrlFunc(url)
  def addCorporateLocation(userId: String, viewId : Long, datePosted : Date, longitude : Double, latitude : Double) : Boolean =
    addCorporateLocationFunc(userId,viewId, datePosted, longitude, latitude)
  def addPhysicalLocation(userId: String, viewId : Long, datePosted : Date, longitude : Double, latitude : Double) : Boolean =
    addPhysicalLocationFunc(userId,viewId, datePosted, longitude, latitude)
}

class TransactionMetadataImpl(
  narative : String,
  comments_ : List[Comment],
  saveOwnerComment : String => Unit,
  addCommentFunc : (String,Long, String, Date) => Comment,
  tags_ : List[Tag],
  addTagFunc : (String, Long, String, Date) => Tag,
  deleteTagFunc : (String) => Unit,
  images_ : List[TransactionImage],
  addImageFunc : (String, Long, String, Date, URL) => TransactionImage,
  deleteImageFunc : String => Unit,
  addWhereTagFunc : (String, Long, Date, Double, Double) => Boolean,
  whereTags_ : List[GeoTag]
) extends TransactionMetadata with Loggable {

  def ownerComment = Some(narative)
  def ownerComment(comment : String) = saveOwnerComment(comment)
  def comments : List[Comment] = comments_
  def addComment(userId: String, viewId : Long, text: String, datePosted : Date) : Comment =
    addCommentFunc(userId, viewId, text, datePosted)
  def tags = tags_
  def addTag(userId : String, viewId : Long, tag : String, postedDate : Date) : Tag =
    addTagFunc(userId,viewId,tag, postedDate)
  def deleteTag(id : String) : Unit =
    deleteTagFunc(id)
  def images = images_
  def addImage(userId: String, viewId : Long, description: String, datePosted : Date, imageUrl : URL) =
    addImageFunc(userId, viewId, description, datePosted, imageUrl)
  def deleteImage(id : String) = deleteImageFunc(id)
  def addWhereTag(userId : String, viewId : Long, datePosted : Date, longitude : Double, latitude : Double) =
    addWhereTagFunc(userId, viewId, datePosted, longitude, latitude)
  def whereTags = whereTags_
}