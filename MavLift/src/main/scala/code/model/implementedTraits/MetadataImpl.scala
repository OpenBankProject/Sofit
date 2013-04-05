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
import code.model.dataAccess.{OtherAccount, OBPComment}
import net.liftweb.common.Loggable
import java.util.Date
import code.model.traits.TransactionImage
import java.net.URL

class OtherBankAccountMetadataImpl(
  _publicAlias : String,
  _privateAlias : String,
  _moreInfo : String,
  _url : String,
  _imageUrl : String,
  _openCorporatesUrl : String) extends OtherBankAccountMetadata {

   def publicAlias : String = _publicAlias
   def privateAlias : String = _privateAlias
   def moreInfo : String = _moreInfo
   def url : String = _url
   def imageUrl : String = _imageUrl
   def openCorporatesUrl : String = _openCorporatesUrl
}

class TransactionMetadataImpl(
  narative : String,
  comments_ : List[Comment],
  saveOwnerComment : String => Unit,
  addCommentFunc : (String,Long, String, Date) => Unit,
  tags_ : List[Tag],
  addTagFunc : (String, Long, String, Date) => String,
  deleteTagFunc : (String) => Unit,
  images_ : List[TransactionImage],
  addImageFunc : (String, Long, String, Date, URL) => String,
  deleteImageFunc : String => Unit,
  addWhereTagFunc : (Double, Double) => Unit,
  whereTag_ : (Double, Double)
)
  extends TransactionMetadata with Loggable
{
  def addWhereTag(longitude : Double, latitude : Double) = addWhereTagFunc(longitude,latitude)
  def whereTag = whereTag_
  def ownerComment = if(! narative.isEmpty) Some(narative) else None
  def ownerComment(comment : String) = saveOwnerComment(comment)
  def comments : List[Comment] = comments_
  def addComment(userId: String, viewId : Long, text: String, datePosted : Date) : Unit =
    addCommentFunc(userId, viewId, text, datePosted)
  def tags = tags_
  def addTag(userId : String, viewId : Long, tag : String, postedDate :Date) : String =
    addTagFunc(userId,viewId,tag, postedDate)
  def deleteTag(id : String) : Unit =
    deleteTagFunc(id)
  def images = images_
  def addImage(userId: String, viewId : Long, description: String, datePosted : Date, imageUrl : URL) =
    addImageFunc(userId, viewId, description, datePosted, imageUrl)
  def deleteImage(id : String) = deleteImageFunc(id)
}

