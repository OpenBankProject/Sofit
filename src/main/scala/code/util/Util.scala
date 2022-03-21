package code.util

import code.lib.ObpAPI
import code.lib.ObpJson.UserCustomerLinkJson
import code.util.Helper.MdcLoggable
import net.liftweb.common.{Box, Empty, Full}

object Util extends MdcLoggable {
  def correlatedUserFlow(correlatedUserId: String): Box[UserCustomerLinkJson] = {
    logger.debug("Hello from the correlatedUserFlow, Correlated User ID: " + correlatedUserId)
    ObpAPI.currentUser match {
      case Full(currentUser) =>
        val currentUserId: String = currentUser.user_id
        val bankId = "user." + currentUserId
        ObpAPI.getOrCreateCustomer(bankId, legalName = currentUser.username) match {
          case Full(customerId) =>
            logger.debug("Before create user customer link Customer ID: " + " Current User ID: " + currentUser.user_id)
            ObpAPI.createUserCustomerLink(bankId, currentUserId, customerId)
            logger.debug("Before create user customer link Customer ID: " + customerId + " Correlated User ID: " + correlatedUserId)
            ObpAPI.createUserCustomerLink(bankId, correlatedUserId, customerId)
          case someIssue =>
            logger.error("Correlated User Flow Error: " + someIssue)
            Empty
        }
      case _ =>
        logger.debug("Correlated User Flow - user is not logged in")
        Empty
    }
  }
}
