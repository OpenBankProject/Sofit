package code.util

import net.liftweb.common.{Failure, Full, Box, Loggable}
import net.liftweb.util.{Mailer, Props}

object MyExceptionLogger extends Loggable{
  import net.liftweb.http.Req

  def unapply(in: (Props.RunModes.Value, Req, Throwable)): Option[(Props.RunModes.Value, Req, Throwable)] = {
    import net.liftweb.util.Helpers.now
    import Mailer.{From, To, Subject, PlainMailBodyType}

    val outputStream = new java.io.ByteArrayOutputStream
    val printStream = new java.io.PrintStream(outputStream)
    in._3.printStackTrace(printStream)
    val currentTime = now.toString
    val stackTrace = new String(outputStream.toByteArray)
    val error = currentTime + ": " + stackTrace
    val host = Props.get("base_url", "unknown host")

    val mailSent = for {
      from <- Props.get("mail.exception.sender.address") ?~ "Could not send mail: Missing props param for 'from'"
      // no spaces, comma separated e.g. mail.api.consumer.registered.notification.addresses=notify@example.com,notify2@example.com,notify3@example.com
      toAddressesString <- Props.get("mail.exception.registered.notification.addresses") ?~ "Could not send mail: Missing props param for 'to'"
    } yield {

      //technically doesn't work for all valid email addresses so this will mess up if someone tries to send emails to "foo,bar"@example.com
      val to = toAddressesString.split(",").toList
      val toParams = to.map(To(_))
      val params = PlainMailBodyType(error) :: toParams

      //this is an async call
      Mailer.sendMail(
        From(from),
        Subject("you got an exception on "+host),
        params :_*
      )
    }

    //if Mailer.sendMail wasn't called (note: this actually isn't checking if the mail failed to send as that is being done asynchronously)
    if(mailSent.isEmpty)
      logger.warn("Exception notification failed: " +mailSent)

     None
  }
}