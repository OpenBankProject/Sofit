package code.util

import code.lib.ObpJson.AccountJson


object Helper {



/*
Returns a string which can be used for the title of the account
*/
  def getAccountTitle(accountJson: AccountJson ) : String = {

  // TODO rewrite in more idiomatic style
      var title = accountJson.label.getOrElse("")
      if (title.isEmpty) {
        if (hasManagementAccess(accountJson))
          title = accountJson.number.getOrElse("")
        else
          title = accountJson.id.getOrElse("")
      }
    title
  }


  def hasManagementAccess (accountJson: AccountJson ) : Boolean  = {
    val availableViews = accountJson.views_available.toList.flatten
    availableViews.exists(view => view.id == Some("owner"))
  }





}
