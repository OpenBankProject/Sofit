/**
Open Bank Project - API
Copyright (C) 2011-2015, TESOBE GmbH.

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
TESOBE GmbH.
Osloer Str. 16/17
Berlin 13359, Germany


  */

package code.snippet

import code.util.Helper.MdcLoggable
import net.liftweb.util.{CssSel, Props}
import net.liftweb.util._
import Helpers._
import code.Constant.{linkBetweenCorrelatedUserAndCustomerCreatedCookieName, _}
import code.lib.ObpAPI
import code.lib.ObpAPI._
import net.liftweb.common.Full
import net.liftweb.http.S
import net.liftweb.json.{Extraction, compactRender}




class WebUI extends MdcLoggable{

  def setChangePasswordLink = {
    val host = Props.get("api_portal_hostname").or(Props.get("api_hostname")).getOrElse("unknown")
    "#change-password-link a [href]" #> scala.xml.Unparsed(s"$host/user_mgt/change_password")
  }
  
  def hideFooter: CssBindFunc = {
    val display: Boolean = S.request.map(_.uri) match {
      case Full("/") => true
      case Full("/index") => true
      case Full("/index.html") => true
      case _ => false
    }
    if(false) "#footer-about [style]" #> "visibility: visible;" else "#footer-about [style]" #> "display: none;"
  }
  
  def homePage = {
    val host = Props.get("base_url").getOrElse("unknown")
    "#uk a [href]" #> scala.xml.Unparsed(s"$host/?locale=en_GB") &
    "#it a [href]" #> scala.xml.Unparsed(s"$host/?locale=it_IT") &
    "#ua a [href]" #> scala.xml.Unparsed(s"$host/?locale=uk_UA")
  }
  
  def setForgottenPasswordLink = {
    val host = Props.get("api_portal_hostname").or(Props.get("api_hostname")).getOrElse("unknown")
    "#reset-password-link a [href]" #> scala.xml.Unparsed(s"$host/user_mgt/lost_password")
  }
  
  def hideTwitterLink = {
    val display: Boolean = Props.getBool("display_twitter_link", true)
    if(display) "#twitter_link [style]" #> "visibility: visible;" else "#twitter_link [style]" #> "display: none;"
  }  
  def hideGitHubLink = {
    val display: Boolean = Props.getBool("display_github_link", true)
    if(display) "#github_link [style]" #> "visibility: visible;" else "#github_link [style]" #> "display: none;"
  }  
  def hideApiDocsLink = {
    val display: Boolean = Props.getBool("display_api_docs_link", true)
    if(display) "#api_documentation_link [style]" #> "visibility: visible;" else "#api_documentation_link [style]" #> "display: none;"
  }
  
  def headerLogoLeft = {
    "img [src]" #> Props.get("webui_header_logo_left_url", "/media/images/logo-header.png")
  }
  
  // Note: Most of these are not used yet
  
  def headerLogoRight: CssSel = {
    "img [src]" #> Props.get("webui_header_logo_right_url", "")
  }

  def aboutBackground: CssSel = {
    "#main-about [style]" #> ("background-image: url(" + Props.get("webui_index_page_about_section_background_image_url", "") + ");")
  }

  def aboutText: CssSel = {
    ".about-text *" #> scala.xml.Unparsed(Props.get("webui_index_page_about_section_text", ""))
  }

  def apiExplorerLink: CssSel = {
    ".api-explorer-link a [href]" #> scala.xml.Unparsed(Props.get("webui_api_explorer_url", "")) &
      hideApiDocsLink
  }


  def mainStyleSheet: CssSel = {
    "#main_style_sheet [href]" #> scala.xml.Unparsed(Props.get("webui_main_style_sheet", "/media/css/website.css"))
  }

  def overrideStyleSheet: CssSel = {
    "#override_style_sheet [href]" #> scala.xml.Unparsed(Props.get("webui_override_style_sheet", ""))
  }

  // Used to represent partners or sponsors of this API instance
  case class Partner(
                      logoUrl: String,
                      homePageUrl: String,
                      altText: String
                      )

  // Builds the grid of images / links for partners on the home page
  def createMainPartners: CssSel = {

    import net.liftweb.json._
    implicit val formats = DefaultFormats


    val mainPartners: Option[String] = {
      Props.get("webui_main_partners")
    }


    val partners = mainPartners match {
      // If we got a value, and can parse the text to Json AST and then extract List[Partner], do that!
      // We expect the following Json string in the Props
      // webui_main_partners=[{"logoUrl":"www.example.com/logoA.png", "homePageUrl":"www.example.com/indexA.html", "altText":"Alt Text A"},{"logoUrl":"www.example.com/logoB.png", "homePageUrl":"www.example.com/indexB.html", "altText":"Alt Text B"}]

      case Some(ps) => {
        try {
          // Parse the Json string into Json AST and then extract a list of Partners
          (parse(ps)).extract[List[Partner]]
        }
        catch {
          case e: Exception => {
            logger.warn(s"You provided a value for webui_main_partners in your Props file but I can't parse it / extract it to a List[Partner]: Exception is: $e")
            Nil
          }
        }
      }
      case _ => Nil
    }

    // Select the "a" tag inside the "main-partners" div and generate a link for each partner
    "#main-partners a" #> partners.map { i =>
      "a [href]" #> s"${i.homePageUrl}" &
        "img [src]" #> s"${i.logoUrl}" &
        "img [alt]" #> s"${i.altText}"
    }
  }

  def helpInfo = {
    val link = scala.xml.Unparsed(Props.get("eula_url", "#"))
    val text = scala.xml.Unparsed(Props.get("eula_text", "End User Licence Agreement & Privacy Policy"))
    "#help-page-doc-link [href]" #> link &
    "#help-page-doc-link *" #> text
  }
  def sourceCodeLink = {
    val link = scala.xml.Unparsed(Props.get("source_code_url", "https://github.com/OpenBankProject/Sofit"))
    "#about-source-code-link [href]" #> link &
    "#about-source-code-link *" #> link
  }
  def sourceCodeText = {
    val text = scala.xml.Unparsed(S.?("source_code_text"))
    "#about-source-code-text *" #> text
  }

  def debugInfo = {

    val correlatedUserIdTargetCookieValue = correlatedUserIdTargetCookieName + ": " + S.cookieValue(correlatedUserIdTargetCookieName).getOrElse("does not exist")
    val correlatedUserIdBoundCookieValue = correlatedUserIdBoundCookieName + ": " + S.cookieValue(correlatedUserIdBoundCookieName).getOrElse("does not exist")
    val correlatedCustomerIdCreatedCookieValue = correlatedCustomerIdCreatedCookieName + ": " + S.cookieValue(correlatedCustomerIdCreatedCookieName).getOrElse("does not exist")
    val linkBetweenCorrelatedUserAndCustomerCreatedCookieValue = linkBetweenCorrelatedUserAndCustomerCreatedCookieName + ": " + S.cookieValue(linkBetweenCorrelatedUserAndCustomerCreatedCookieName).getOrElse("does not exist")
    val correlatedEntities = ObpAPI.getMyCorrelatedEntities
    val correlatedEntitiesJson = correlatedEntities
      .map(correlatedEntities => "CORRELATED_ENTITIES: " + compactRender(Extraction.decompose(correlatedEntities)))
      .getOrElse("CORRELATED_ENTITIES: does not exist")
    
    "#correlatedUserIdTargetCookieName *" #> correlatedUserIdTargetCookieValue &
    "#correlatedUserIdBoundCookieName *" #> correlatedUserIdBoundCookieValue &
    "#correlatedCustomerIdCreatedCookieName *" #> correlatedCustomerIdCreatedCookieValue &
    "#linkBetweenCorrelatedUserAndCustomerCreatedCookieName *" #> linkBetweenCorrelatedUserAndCustomerCreatedCookieValue &
    "#correlatedEntities" #> correlatedEntitiesJson
    
  }

}