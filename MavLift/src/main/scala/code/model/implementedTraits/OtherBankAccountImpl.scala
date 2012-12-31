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

import code.model.traits.{OtherBankAccountMetadata,OtherBankAccount}
import code.model.dataAccess.OBPAccount
import code.model.dataAccess.Account
import code.model.dataAccess.OtherAccount

class OtherBankAccountImpl(theAccount: Account, otherAccount: OBPAccount) extends OtherBankAccount
{
  
  private val oAccs = theAccount.otherAccounts.get
  val otherUnmediatedHolder = otherAccount.holder.get.toString
  val oAccOpt = oAccs.find(o => {
    otherUnmediatedHolder.equals(o.holder.get)
  })

  val oAcc = oAccOpt getOrElse {
    OtherAccount.createRecord
  }
  
  val id = ""
  val label = otherAccount.holder.get.toString
  val nationalIdentifier = otherAccount.bank.get.national_identifier.get
  val swift_bic = None //TODO: need to add this to the json/model
  val iban = Some(otherAccount.bank.get.IBAN.get)
  val number = otherAccount.number.get.toString
  val bankName = "" //TODO: need to add this to the json/model
  val metadata = new OtherBankAccountMetadataImpl(oAcc.publicAlias.get, oAcc.privateAlias.get, oAcc.moreInfo.get,
      oAcc.url.get, oAcc.imageUrl.get, oAcc.openCorporatesUrl.get)
}