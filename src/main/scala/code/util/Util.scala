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
  def correlatedUserFlow(correlatedUserId: String): Boolean = {
    logger.debug("Hello from the correlatedUserFlow, Correlated User ID: " + correlatedUserId)
    ObpAPI.currentUser match {
      case Full(currentUser) =>
        val currentUserId: String = currentUser.user_id
        val bankId = "user." + currentUserId
        ObpAPI.getOrCreateCustomer(bankId, legalName = currentUser.username) match {
          case Full(customerId) =>
            S.addCookie(HTTPCookie(Constant.correlatedCustomerIdCreatedCookieName, customerId))
            logger.debug("Before create user customer link Customer ID: " + " Current User ID: " + currentUser.user_id)
            val loggedOnUserIdDone = ObpAPI.createUserCustomerLinkIfDoesNotExists(bankId, currentUserId, customerId)
            if(loggedOnUserIdDone) S.addCookie(HTTPCookie(Constant.linkBetweenCorrelatedUserAndCustomerCreatedCookieName, currentUserId))
            logger.debug("Before create user customer link Customer ID: " + customerId + " Correlated User ID: " + correlatedUserId)
            val correlatedUserIdDone = ObpAPI.createUserCustomerLinkIfDoesNotExists(bankId, correlatedUserId, customerId)
            if(loggedOnUserIdDone) S.addCookie(HTTPCookie(Constant.correlatedUserIdBoundCookieName, correlatedUserId))
            if(loggedOnUserIdDone && correlatedUserIdDone) {
              S.deleteCookie(Constant.correlatedUserIdTargetCookieName)
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
  def currentLocale() : Locale = {
    val locale = Locale.getAvailableLocales().toList.filter { l =>
      l.toLanguageTag == Props.get("language_tag", "en-GB")
    }.head
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
