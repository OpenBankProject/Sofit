package code.snippet

import java.text.SimpleDateFormat
import java.util.Calendar

import net.liftweb.util.Helpers._

/**
 * So we can put the current year in a template
 */

class TimeHelpers {

  val now = Calendar.getInstance().getTime()
  // create the date/time formatter
  val yearFormat = new SimpleDateFormat("yyyy")
  val nowYear = yearFormat.format(now)

  def currentYear = "#current_year" #> nowYear.toString
}
