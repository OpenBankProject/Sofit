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

import code.model.traits.{Comment, OtherBankAccountMetadata, TransactionMetadata}
import code.model.dataAccess.{OtherAccount, OBPComment}
import net.liftweb.common.Loggable
import java.util.Date

class OtherBankAccountMetadataImpl(oAcc: OtherAccount) extends OtherBankAccountMetadata {

   def publicAlias : String = oAcc.publicAlias.get
   def privateAlias : String = oAcc.privateAlias.get
   def moreInfo : String = oAcc.moreInfo.get
   def url : String = oAcc.url.get
   def imageUrl : String = oAcc.imageUrl.get
   def openCorporatesUrl : String = oAcc.openCorporatesUrl.get
}

class TransactionMetadataImpl(narative : String, comments_ : List[Comment], 
  saveOwnerComment : String => Unit, addCommentFunc : (Long,Long, String, Date) => Unit ) 
  extends TransactionMetadata with Loggable
{
  def ownerComment = if(! narative.isEmpty) Some(narative) else None
  def ownerComment(comment : String) = saveOwnerComment(comment)
  def comments : List[Comment] = comments_
  def addComment(userId: Long, viewId : Long, text: String, datePosted : Date) : Unit = 
    addCommentFunc(userId, viewId, text, datePosted)
}

