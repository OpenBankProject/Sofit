package code.util

import java.text.SimpleDateFormat
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import java.util.{Calendar, Date, Locale}

import code.Constant
import code.lib.ObpAPI
import code.util.Helper.MdcLoggable
import net.liftweb.common.Full
import net.liftweb.http.S
import net.liftweb.http.provider.HTTPCookie
import net.liftweb.util.Props

import scala.util.Try

object Util extends MdcLoggable {
  def correlatedUserFlow(correlatedUserId: Option[String]): String = {
    logger.debug("Hello from the correlatedUserFlow, Correlated User ID: " + correlatedUserId)
    ObpAPI.currentUser match {
      case Full(currentUser) =>
        val currentUserId: String = currentUser.user_id
        val bankId = "user." + currentUserId
        ObpAPI.getOrCreateCustomer(bankId, legalName = currentUser.username) match {
          case Full(customerId) =>
            S.addCookie(HTTPCookie(Constant.correlatedCustomerIdCreatedCookieName, customerId))
            // TODO refactor this so that we check if there are links instead of relying on the error message.
            logger.debug(s"Before createUserCustomerLinkIfDoesNotExists bankid: $bankId currentUserId: $currentUserId customerId: $customerId")
            val loggedOnUserIdDone = ObpAPI.createUserCustomerLinkIfDoesNotExists(bankId, currentUserId, customerId)
            if(loggedOnUserIdDone) S.addCookie(HTTPCookie(Constant.linkBetweenCorrelatedUserAndCustomerCreatedCookieName, currentUserId))

            correlatedUserId match {
              case Some(localCorrelatedUserId) =>
                logger.debug("Before create user customer link Customer ID: " + customerId + " Correlated User ID: " + correlatedUserId)
                val correlatedUserIdDone = ObpAPI.createUserCustomerLinkIfDoesNotExists(bankId, localCorrelatedUserId, customerId)
                if(loggedOnUserIdDone) S.addCookie(HTTPCookie(Constant.correlatedUserIdBoundCookieName, localCorrelatedUserId)) // Seems this cookie is only for info purposes
                if(loggedOnUserIdDone && correlatedUserIdDone) {
                  S.deleteCookie(Constant.correlatedUserIdTargetCookieName)
                  "loggedOnUserIdDone&&correlatedUserIdDone"
                } else {
                  "NOT (loggedOnUserIdDone&&correlatedUserIdDone)"
                }
              case _ =>
                logger.debug("No correlated user_id supplied, Not creating correlated user.")
                "no correlatedUserId just customer"
            }

          case someIssue =>
            logger.error("Correlated User Flow Error: " + someIssue)
            "Error"
        }
      case _ =>
        logger.debug("Correlated User Flow - user is not logged in")
        "NotLoggedIn"
    }
  }
  
  def monthsAgo(months: Int): Date = {
    val currentDate = new Date()
    val calendar = Calendar.getInstance
    calendar.setTime(currentDate)
    calendar.add(Calendar.MONTH, -months)
    val pastDate: Date = calendar.getTime
    pastDate
  }

  def getLocalDate(date: Date): String = {
    import java.text.DateFormat
    val df = DateFormat.getDateInstance(DateFormat.LONG, currentLocale())
    val formattedDate = df.format(date)
    formattedDate
  }

  def getLocale(): Locale = Locale.getAvailableLocales().toList.filter { l =>
    l.toLanguageTag == Props.get("language_tag", "en-GB")
  }.headOption.getOrElse(Locale.ENGLISH)
  
  def currentLocale() : Locale = {
    val locale = getLocale()
    // Cookie name
    val localeCookieName = "SELECTED_LOCALE"
    S.findCookie(localeCookieName).flatMap {
      cookie => cookie.value.map(computeLocale)
    } openOr locale
  }
  // Properly convert a language tag to a Locale
  def computeLocale(tag : String) = tag.split(Array('-', '_')) match {
    case Array(lang) => new Locale(lang)
    case Array(lang, country) => new Locale(lang, country)
    case Array(lang, country, variant) => new Locale(lang, country, variant)
  }
  
  def getTimeAgo(date: Date): String = {
    import com.github.marlonlom.utilities.timeago.TimeAgo
    import com.github.marlonlom.utilities.timeago.TimeAgoMessages
    val messages = new TimeAgoMessages.Builder().withLocale(currentLocale()).build
    val timeInMillis = date.getTime()
    val text = TimeAgo.using(timeInMillis, messages)
    text
  }
  
}
