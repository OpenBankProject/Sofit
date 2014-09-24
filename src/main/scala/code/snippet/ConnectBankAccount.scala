package code.snippet

import code.lib.OAuthClient
import net.liftweb.http.{S, SHtml}
import net.liftweb.util.{PassThru, CssSel}
import net.liftweb.util.Helpers._

import scala.xml.NodeSeq

class ConnectBankAccount {

  def connect : NodeSeq => NodeSeq = {

    val currentPage = S.uri

    //only show something if we're on the home page
    if(currentPage == "/" || currentPage == "/index") {
      ".navlink [onclick+]" #> SHtml.onEvent(_ => {
        OAuthClient.redirectToConnectBankAccount()
      })
    } else {
      "* *" #> NodeSeq.Empty
    }

  }

}
