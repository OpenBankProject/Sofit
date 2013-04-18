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

package code.model.dataAccess

import com.mongodb.QueryBuilder
import net.liftweb.mongodb.JsonObjectMeta
import net.liftweb.mongodb.JsonObject
import net.liftweb.mongodb.record.MongoMetaRecord
import net.liftweb.mongodb.record.field.ObjectIdPk
import net.liftweb.mongodb.record.field.ObjectIdRefListField
import net.liftweb.mongodb.record.MongoRecord
import net.liftweb.mongodb.record.field.ObjectIdRefField
import net.liftweb.mongodb.record.field.MongoJsonObjectListField
import net.liftweb.mongodb.record.field.DateField
import net.liftweb.common.{ Box, Empty, Full }
import net.liftweb.mongodb.record.field.BsonRecordListField
import net.liftweb.mongodb.record.{ BsonRecord, BsonMetaRecord }
import net.liftweb.record.field.{ StringField, BooleanField, DecimalField }
import net.liftweb.mongodb.{Limit, Skip}
import code.model.dataAccess.OBPEnvelope._
import code.model.traits.ModeratedTransaction
import code.model.traits.BankAccount
import code.model.implementedTraits.{ BankAccountImpl, AccountOwnerImpl }
import net.liftweb.mongodb.BsonDSL._
import java.util.Date


/**
 * There should be only one of these for every real life "this" account. TODO: Enforce this
 *
 * As a result, this can provide a single point from which to retrieve the aliases associated with
 * this account, rather than needing to duplicate the aliases into every single transaction.
 */

class Account extends MongoRecord[Account] with ObjectIdPk[Account] {
  def meta = Account

  object balance extends DecimalField(this, 0)
  object anonAccess extends BooleanField(this, false)
  object holder extends StringField(this, 255)
  object number extends StringField(this, 255)
  object kind extends StringField(this, 255)
  object name extends StringField(this, 255)
  object permalink extends StringField(this, 255)
  object bankID extends ObjectIdRefField(this, HostedBank)
  object label extends StringField(this, 255)
  object currency extends StringField(this, 255)
  object iban extends StringField(this, 255)
  object lastUpdate extends DateField(this)
  object otherAccounts extends ObjectIdRefListField(this, OtherAccount)

  def bankName : String = bankID.obj match  {
    case Full(bank) => bank.name.get
    case _ => ""
  }
  def bankPermalink : String  = bankID.obj match  {
    case Full(bank) => bank.permalink.get
    case _ => ""
  }

  def transactionsForAccount = QueryBuilder.start("obp_transaction.this_account.number").is(number.get).
    put("obp_transaction.this_account.kind").is(kind.get).
    put("obp_transaction.this_account.bank.name").is(bankName)

  //find all the envelopes related to this account
  def allEnvelopes: List[OBPEnvelope] = OBPEnvelope.findAll(transactionsForAccount.get)

  def envelopes(queryParams: OBPQueryParam*): List[OBPEnvelope] = {
    val DefaultSortField = "obp_transaction.details.completed"

    val limit = queryParams.collect { case OBPLimit(value) => value }.headOption.getOrElse(50)
    val offset = queryParams.collect { case OBPOffset(value) => value }.headOption.getOrElse(0)
    val orderingParams = queryParams.collect { case param: OBPOrdering => param}.headOption
      .getOrElse(OBPOrdering(Some(DefaultSortField), OBPDescending))

    val fromDate = queryParams.collect { case param: OBPFromDate => param }.headOption
    val toDate = queryParams.collect { case param: OBPFromDate => param }.headOption

    val mongoParams = {
      val start = transactionsForAccount
      val start2 = if(fromDate.isDefined) start.put("obp_transaction.details.completed").greaterThanEquals(fromDate.get.value)
      			   else start
      val end = if(toDate.isDefined) start2.put("obp_transaction.details.completed").lessThanEquals(toDate.get.value)
      			else start2
      end.get
    }

    val ordering =  QueryBuilder.start(orderingParams.field.getOrElse(DefaultSortField)).is(orderingParams.order.orderValue).get

    OBPEnvelope.findAll(mongoParams, ordering, Limit(limit), Skip(offset))
  }
}

object Account extends Account with MongoMetaRecord[Account] {
  def toBankAccount(account: Account): BankAccount = {
    val iban = if (account.iban.toString.isEmpty) None else Some(account.iban.toString)
    var bankAccount = new BankAccountImpl(account.id.toString, Set(), account.kind.toString, account.balance.get, account.currency.toString, account.label.toString,
      "", None, iban, account.anonAccess.get, account.number.get, account.bankName, account.bankPermalink, account.permalink.get)
    val owners = Set(new AccountOwnerImpl("", account.holder.toString, Set(bankAccount)))
    bankAccount.owners = Set(new AccountOwnerImpl("", account.holder.toString, Set(bankAccount)))

    bankAccount
  }
}

class OtherAccount private() extends MongoRecord[OtherAccount] with ObjectIdPk[OtherAccount] {
  def meta = OtherAccount

  object holder extends StringField(this, 200)
  object publicAlias extends StringField(this, 100)
  object privateAlias extends StringField(this, 100)
  object moreInfo extends StringField(this, 100)
  object url extends StringField(this, 100)
  object imageUrl extends StringField(this, 100)
  object openCorporatesUrl extends StringField(this, 100) {
    override def optional_? = true
  }
  //we store a list of geo tags, one per view
  object corporateLocation extends BsonRecordListField(this, OBPGeoTag)
  object physicalLocation extends BsonRecordListField(this, OBPGeoTag)

  def addCorporateLocation(userId: String, viewId : Long, datePosted : Date, longitude : Double, latitude : Double) : Boolean = {
    val newTag = OBPGeoTag.createRecord.
                userId(userId).
                viewID(viewId).
                date(datePosted).
                geoLongitude(longitude).
                geoLatitude(latitude)

    //before to save the geo tag we need to be sure there is only one per view
    //so we look if there is allready a tag with the same view (viewId)
    val tags = corporateLocation.get.find(geoTag => geoTag.viewID equals viewId) match {
      case Some(tag) => {
        //if true remplace it with the new one
        newTag :: corporateLocation.get.diff(Seq(tag))
      }
      case _ =>
        //else just add this one
        newTag :: corporateLocation.get
    }
    corporateLocation(tags).save
    true
  }
  def addPhysicalLocation(userId: String, viewId : Long, datePosted : Date, longitude : Double, latitude : Double) : Boolean = {
    val newTag = OBPGeoTag.createRecord.
                userId(userId).
                viewID(viewId).
                date(datePosted).
                geoLongitude(longitude).
                geoLatitude(latitude)

    //before to save the geo tag we need to be sure there is only one per view
    //so we look if there is allready a tag with the same view (viewId)
    val tags = physicalLocation.get.find(geoTag => geoTag.viewID equals viewId) match {
      case Some(tag) => {
        //if true remplace it with the new one
        newTag :: physicalLocation.get.diff(Seq(tag))
      }
      case _ =>
        //else just add this one
        newTag :: physicalLocation.get
    }
    physicalLocation(tags).save
    true
  }

}

object OtherAccount extends OtherAccount with MongoMetaRecord[OtherAccount]

class HostedBank extends MongoRecord[HostedBank] with ObjectIdPk[HostedBank]{
  def meta = HostedBank

  object name extends StringField(this, 255)
  object alias extends StringField(this, 255)
  object logoURL extends StringField(this, 255)
  object website extends StringField(this, 255)
  object email extends StringField(this, 255)
  object permalink extends StringField(this, 255)
  object SWIFT_BIC extends StringField(this, 255)
  object national_identifier extends StringField(this, 255)

  def getAccount(bankAccountPermalink : String) : Box[Account] =
    Account.find(("permalink" -> bankAccountPermalink) ~ ("bankID" -> id.is))

  def isAccount(bankAccountPermalink : String) : Boolean =
    Account.count(("permalink" -> bankAccountPermalink) ~ ("bankID" -> id.is)) == 1

}

object HostedBank extends HostedBank with MongoMetaRecord[HostedBank]
