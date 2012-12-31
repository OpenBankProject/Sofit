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

import code.model.dataAccess.{OBPEnvelope,OBPTransaction,OtherAccount}
import code.model.traits.{Transaction,BankAccount,OtherBankAccount, TransactionMetadata}
import scala.math.BigDecimal
import java.util.Date
import scala.collection.immutable.List
import net.liftweb.common.Loggable
import net.liftweb.common.Box
import code.model.traits.Comment
import code.model.dataAccess.Account

class TransactionImpl(env: OBPEnvelope, theAccount: Account) extends Transaction {

  val transaction: OBPTransaction = env.obp_transaction.get
  val otherAccount_ = transaction.other_account.get
  val otherUnmediatedHolder = otherAccount_.holder.get

  var thisAccount = Account.toBankAccount(theAccount)

  private val oAccs = theAccount.otherAccounts.get
  val oAccOpt = oAccs.find(o => {
    otherUnmediatedHolder.equals(o.holder.get)
  })

  val oAcc = oAccOpt getOrElse {
    OtherAccount.createRecord
  }

  val id = env.id.is.toString()
  val oSwiftBic = None
  val otherAccount = new OtherBankAccountImpl(
    id_ = "",
    label_ = otherAccount_.holder.get,
    nationalIdentifier_ = otherAccount_.bank.get.national_identifier.get,
    swift_bic_ = None, //TODO: need to add this to the json/model
    iban_ = Some(otherAccount_.bank.get.IBAN.get),
    number_ = otherAccount_.number.get,
    bankName_ = "", //TODO: need to add this to the json/model
    metadata_ = new OtherBankAccountMetadataImpl(oAcc.publicAlias.get, oAcc.privateAlias.get, oAcc.moreInfo.get,
      oAcc.url.get, oAcc.imageUrl.get, oAcc.openCorporatesUrl.get))
  val metadata = new TransactionMetadataImpl(env.narrative.get, env.obp_comments.objs,
    (text => env.narrative(text).save), env.addComment _)
  val transactionType = env.obp_transaction.get.details.get.type_en.get
  val amount = env.obp_transaction.get.details.get.value.get.amount.get
  val currency = env.obp_transaction.get.details.get.value.get.currency.get
  val label = None
  val startDate = env.obp_transaction.get.details.get.posted.get
  val finishDate = env.obp_transaction.get.details.get.completed.get
  val balance = env.obp_transaction.get.details.get.new_balance.get.amount.get
}