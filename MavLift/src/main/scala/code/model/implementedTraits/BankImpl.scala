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

import code.model.traits.{Bank, BankAccount, User}
import code.model.dataAccess.LocalStorage
import code.model.dataAccess.HostedBank

import net.liftweb.common.Box

class BankImpl(
  _id: String,
  _shortName : String,
  _fullName : String,
  _permalink : String,
  _logoURL : String,
  _website : String
) extends Bank
{
	def id = _id
	def shortName = _shortName
  def fullName = _fullName
  def logoURL = _logoURL
	def permalink = _permalink
	def accounts = LocalStorage.getBankAccounts(this)
  def publicAccounts = LocalStorage.getPublicBankAccounts(this)
  def privateAccounts(user : Box[User]) : Set[BankAccount] = {
    //ask the localStorage for the all the private accounts
    val accounts = LocalStorage.getPrivateBankAccounts(this)
    //then see if for every one there is at least a view available for the user

    def atLeastOneView(account : BankAccount, user : Box[User]) = {
      ! account.permittedViews(user).isEmpty
    }

    accounts.filter(atLeastOneView(_,user))
  }
  def website = _website
}
