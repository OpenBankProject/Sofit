package code.util

import code.lib.ObpAPI._
import code.lib.ObpJson.AccountJson
import net.liftweb.common.Full


object Helper {



  // From bankId / accountId
  def getAccountTitle (bankId: String, accountId: String) : String = {
    val accountJsonBox = getAccount(bankId, accountId, "owner")

    val accountTitle = accountJsonBox match {
      case Full(accountJson) => getAccountTitle(accountJson)
      case _ => "Unknown Account"
    }
    accountTitle
  }


/*
Returns a string which can be used for the title of the account
Uses the Label else Id if possible
*/
  def getAccountTitle(accountJson: AccountJson ) : String = {
    accountJson.label.getOrElse(accountJson.id.getOrElse("---"))
  }


  def hasManagementAccess (accountJson: AccountJson ) : Boolean  = {
    val availableViews = accountJson.views_available.toList.flatten
    availableViews.exists(view => view.id == Some("owner"))
  }





}
