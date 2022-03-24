package code.util

import code.Constant
import code.lib.ObpAPI
import code.util.Helper.MdcLoggable
import net.liftweb.common.Full
import net.liftweb.http.S
import net.liftweb.http.provider.HTTPCookie

object Util extends MdcLoggable {
  def correlatedUserFlow(correlatedUserId: String): Boolean = {
    logger.debug("Hello from the correlatedUserFlow, Correlated User ID: " + correlatedUserId)
    ObpAPI.currentUser match {
      case Full(currentUser) =>
        val currentUserId: String = currentUser.user_id
        val bankId = "user." + currentUserId
        ObpAPI.getOrCreateCustomer(bankId, legalName = currentUser.username) match {
          case Full(customerId) =>
            S.addCookie(HTTPCookie(Constant.correlatedCustomerIdCookieName, customerId))
            logger.debug("Before create user customer link Customer ID: " + " Current User ID: " + currentUser.user_id)
            val loggedOnUserIdDone = ObpAPI.createUserCustomerLinkIfDoesNotExists(bankId, currentUserId, customerId)
            if(loggedOnUserIdDone) S.addCookie(HTTPCookie(Constant.loggedOnUserIdCookieName, currentUserId))
            logger.debug("Before create user customer link Customer ID: " + customerId + " Correlated User ID: " + correlatedUserId)
            val correlatedUserIdDone = ObpAPI.createUserCustomerLinkIfDoesNotExists(bankId, correlatedUserId, customerId)
            if(loggedOnUserIdDone) S.addCookie(HTTPCookie(Constant.correlatedUserIdBoundCookieName, correlatedUserId))
            if(loggedOnUserIdDone && correlatedUserIdDone) {
              S.deleteCookie(Constant.correlatedUserIdCookieName)
              true
            } else {
              false
            }
          case someIssue =>
            logger.error("Correlated User Flow Error: " + someIssue)
            false
        }
      case _ =>
        logger.debug("Correlated User Flow - user is not logged in")
        false
    }
  }
}
