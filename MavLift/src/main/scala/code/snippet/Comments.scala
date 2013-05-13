/**
Open Bank Project - Transparency / Social Finance Web Application
Copyright (C) 2011, 2012, TESOBE / Music Pictures Ltd

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
TESOBE / Music Pictures Ltd
Osloerstrasse 16/17
Berlin 13359, Germany

  This product includes software developed at
  TESOBE (http://www.tesobe.com/)
  by
  Simon Redfern : simon AT tesobe DOT com
  Stefan Bethge : stefan AT tesobe DOT com
  Everett Sochowski : everett AT tesobe DOT com
  Ayoub Benali: ayoub AT tesobe DOT com

 */
package code.snippet

import net.liftweb.http.js.JsCmds.Noop
import net.liftweb.http.Templates
import net.liftweb.util.Helpers._
import net.liftweb.http.S
import net.liftweb.common.Full
import scala.xml.NodeSeq
import net.liftweb.http.SHtml
import net.liftweb.common.Box
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JE.JsRaw
import net.liftweb.http.js.JsCmds.RedirectTo
import net.liftweb.http.SessionVar
import scala.xml.Text
import net.liftweb.json._
import net.liftweb.json.JsonAST.JObject
import net.liftweb.json.JsonAST.JField
import net.liftweb.json.JsonAST.JString
import net.liftweb.json.JsonAST.JArray
import net.liftweb.http.StringField
import java.util.Date
import java.text.SimpleDateFormat
import net.liftweb.common.Loggable
import code.model.traits.{ModeratedTransaction,PublicAlias,PrivateAlias,NoAlias,Comment, View, Tag, User}
import java.util.Currency
import net.liftweb.http.js.jquery.JqJsCmds.{AppendHtml,Hide}
import net.liftweb.http.js.JsCmds.{SetHtml,SetValById}
import net.liftweb.http.js.JE.Str
import net.liftweb.http.js.JsCmds.Alert
import code.model.traits.TransactionImage
import net.liftweb.util.Props
import scala.xml.Utility
import net.liftweb.common.Failure
import java.net.URL
import java.net.URI
import java.text.NumberFormat
import net.liftweb.common.ParamFailure
import net.liftweb.util.CssSel
import code.lib.ObpJson._
import code.lib.ObpAPI
import sun.reflect.generics.reflectiveObjects.NotImplementedException
import code.lib.OAuthClient
import net.liftweb.http.RequestVar

case class CommentsURLParams(bankId: String, accountId: String, viewId: String, transactionId: String)
/**
 * This whole class is a rather hastily put together mess
 */
class Comments(params : ((ModeratedTransaction, View),(TransactionJson, CommentsURLParams))) extends Loggable{
  
  //TODO: Support multiple providers
  val provider = OAuthClient.defaultProvider
  
  val FORBIDDEN = "---"
  val transaction = params._1._1
  val view = params._1._2
  val transactionInfo = params._2
  val transactionJson = transactionInfo._1
  val urlParams = transactionInfo._2
  val details = transactionJson.details
  val transactionMetaData = transactionJson.metadata
  val otherHolder = transactionJson.other_account.flatMap(_.holder)
  val transactionValue = details.flatMap(_.value)

  def calcCurrencySymbol(currencyCode: Option[String]) = {
    (for {
      code <- currencyCode
      currency <- tryo { Currency.getInstance(code) }
      symbol <- tryo { currency.getSymbol(S.locale) }
    } yield symbol) getOrElse FORBIDDEN
  }
  val commentDateFormat = new SimpleDateFormat("kk:mm:ss EEE MMM dd yyyy")
  val NOOP_SELECTOR = "#i_am_an_id_that_should_never_exist" #> ""

  def commentPageTitle(xhtml: NodeSeq): NodeSeq = {
    val dateFormat = new SimpleDateFormat("EEE MMM dd yyyy")
    var theCurrency = FORBIDDEN
    def formatDate(date: Box[Date]): String = {
      date match {
        case Full(d) => dateFormat.format(d)
        case _ => FORBIDDEN
      }
    }

    (
      ".amount *" #>{
        val amount : String = transactionValue.flatMap(_.amount) getOrElse FORBIDDEN
        val transactionCurrencyCode = transactionValue.flatMap(_.currency)
        val currencySymbol = calcCurrencySymbol(transactionCurrencyCode)
        //TODO: Would be nice to get localise this in terms of "." vs "," and the location of the currency symbol
        // (before or after the number)
        amount + " " + currencySymbol
      } &
      ".other_account_holder *" #> {

        def otherHolderSelector = {
          val holderName = otherHolder.flatMap(_.name)
          val isAlias = otherHolder.flatMap(_.is_alias) getOrElse true

          def aliasSelector = {

            def indicatorClass = urlParams.viewId match {
              case "public" => ".alias_indicator [class+]" #> "alias_indicator_public"
              case _ => ".alias_indicator [class+]" #> "alias_indicator_private"
            }
            
            if (isAlias) {
              ".alias_indicator *" #> "(Alias)" &
              indicatorClass
            } else {
              NOOP_SELECTOR
            }
          }
          
          ".the_name" #> holderName &
          aliasSelector
        }
        
        if(otherHolder.isDefined) otherHolderSelector
        else "* *" #> FORBIDDEN
      } &
      ".date_cleared *" #> {
        val finishDate = details.flatMap(_.completed)
        finishDate match {
          case Some(date) => formatDate(Full(date))
          case _ => FORBIDDEN
        }
      } &
      ".label *" #> {
        val narrative = transactionMetaData.flatMap(_.narrative)
        narrative.getOrElse(FORBIDDEN)
      } &
      ".new_balance *" #> {
        val newBalance = details.flatMap(_.new_balance)
        newBalance match {
          case Some(b) => {
            val amount = b.amount getOrElse FORBIDDEN
            val accountCurrencyCode = b.currency
            val currencySymbol = calcCurrencySymbol(accountCurrencyCode)
            amount + " " + currencySymbol
          }
          case _ => FORBIDDEN
        }
      }
    ).apply(xhtml)
  }

  def images = {
    addImage andThen showImages
  }

  def noImages = ".images_list" #> ""
  def imagesNotAllowed = "* *" #> ""

  def imageHtmlId(image: TransactionImageJson) : String = "trans-image-" + image.id.getOrElse("")
  def imageHtmlId2(image: TransactionImage) : String = "trans-image-" + image.id_

  def showImages = {
    
    def deleteImage2(image: TransactionImage) = {
      transaction.metadata.flatMap(_.deleteImage) match {
        case Some(delete) => delete(image.id_)
        case _ => logger.warn("No delete image function found.")
      }

      val jqueryRemoveImage = "$('.image-holder[data-id=\"" + imageHtmlId2(image) + "\"]').remove();"
      JsRaw(jqueryRemoveImage).cmd
    }

    def imagesSelector(imageJsons: List[TransactionImageJson]) = {

      def deleteImage(imageJson: TransactionImageJson) = {
        //TODO: This could be optimised into calling an ajax function with image id as a parameter to avoid
        //storing multiple closures server side (i.e. one client side function maps to on server side function
        //that takes a parameter)
        SHtml.ajaxInvoke(() => {
          imageJson.id match {
            case Some(id) => {
              val deleted = ObpAPI.deleteImage(urlParams.bankId, urlParams.accountId,
              urlParams.viewId, urlParams.transactionId, id)
              if(!deleted) logger.error("Tried to delete an image but it didn't work")
            }
            case _ => logger.warn("Tried to delete an image without an id")
          }
          val jqueryRemoveImage = "$('.image-holder[data-id=\"" + imageHtmlId(imageJson) + "\"]').remove();"
          JsRaw(jqueryRemoveImage).cmd
        })
      }
        
      ".noImages" #> "" &
        ".image-holder" #> imageJsons.map(imageJson => {
          ".image-holder [data-id]" #> imageHtmlId(imageJson) &
            ".trans-image [src]" #> imageJson.URL.getOrElse("") &
            ".image-description *" #> imageJson.label.getOrElse("") &
            ".postedBy *" #> imageJson.user.flatMap(_.display_name).getOrElse("") &
            ".postedTime *" #> imageJson.date.map(commentDateFormat.format(_)).getOrElse("") &
            ".deleteImage [onclick]" #> deleteImage(imageJson)
        })
    }
      
    val imageJsons = transactionJson.imageJsons

    imageJsons match {
      case Some(iJsons) => {
        if(iJsons.isEmpty) noImages
        else imagesSelector(iJsons)
      }
      case _ => imagesNotAllowed
    }

  }
  
  def apiAddImage = {

    //transloadit requires its parameters to be an escaped json string
    val transloadItParams : String = {
      import net.liftweb.json.JsonDSL._
      import net.liftweb.json._

      val authKey = Props.get("transloadit.authkey") getOrElse ""
      val addImageTemplate = Props.get("transloadit.addImageTemplate") getOrElse ""
      val json =
        (
          "auth" -> (
            "key" -> authKey
          )
        ) ~
        ("template_id" -> addImageTemplate)

      Utility.escape(compact(render(json)), new StringBuilder).toString
    }

    if(S.post_?) {
      val description = S.param("description") getOrElse ""
      val viewId = view.id
      val datePosted = now
      val addFunction = for {
        transloadit <- S.param("transloadit") ?~! "No transloadit data received"
        json <- tryo{parse(transloadit)} ?~! "Could not parse transloadit data as json"
        urlString <- tryo{val JString(a) = json \ "results" \\ "url"; a} ?~! {"Could not extract url string from json: " + compact(render(json))}
      } yield {
        () => ObpAPI.addImage(urlParams.bankId, urlParams.accountId, urlParams.viewId, urlParams.transactionId, urlString, description)
      }

      addFunction match {
        case Full(add) => {
          add()
          //kind of a hack, but we redirect to a get request here so that we get the updated transaction (with the new image)
          S.redirectTo(S.uri)
        }
        case Failure(msg, _ , _) => logger.warn("Problem adding new image: " + msg)
        case _ => logger.warn("Problem adding new image")
      }
    }

    val addImageSelector = for {
      user <- User.currentUser ?~ "You need to long before you can add an image"
      metadata <- Box(transaction.metadata) ?~ "You cannot add images to transactions in this view"
      addImageFunc <- Box(metadata.addImage) ?~ "You cannot add images to transaction in this view"
    } yield {
      "#imageUploader [action]" #> S.uri &
      "#imageUploader" #> {
        "name=params [value]" #> transloadItParams
      }
    }

    //TODO: Here we would need to know if this view allows image uploading... but there is no way
    //to get that yet, as far as I can tell
    throw new NotImplementedException()
    
    addImageSelector match {
      case Full(s) => s
      case Failure(msg, _, _) => ".add *" #> msg
      case _ => ".add *" #> ""
    }
  }

  def addImage = {

    //transloadit requires its parameters to be an escaped json string
    val transloadItParams : String = {
      import net.liftweb.json.JsonDSL._
      import net.liftweb.json._

      val authKey = Props.get("transloadit.authkey") getOrElse ""
      val addImageTemplate = Props.get("transloadit.addImageTemplate") getOrElse ""
      val json =
        (
          "auth" -> (
            "key" -> authKey
          )
        ) ~
        ("template_id" -> addImageTemplate)

      Utility.escape(compact(render(json)), new StringBuilder).toString
    }

    if(S.post_?) {
      val description = S.param("description") getOrElse ""
      val viewId = view.id
      val addedImage = for {
        transloadit <- S.param("transloadit") ?~! "No transloadit data received"
        json <- tryo{parse(transloadit)} ?~! "Could not parse transloadit data as json"
        imageUrl <- tryo{val JString(a) = json \ "results" \\ "url"; a} ?~! {"Could not extract url string from json: " + compact(render(json))}
        addedImage <- ObpAPI.addImage(urlParams.bankId, urlParams.accountId, urlParams.viewId, urlParams.transactionId, imageUrl, description)
      } yield addedImage

      addedImage match {
        case Full(added) => {
          //kind of a hack, but we redirect to a get request here so that we get the updated transaction (with the new image)
          S.redirectTo(S.uri)
        }
        case Failure(msg, _ , _) => logger.warn("Problem adding new image: " + msg)
        case _ => logger.warn("Problem adding new image")
      }
    }

    if (OAuthClient.loggedInAt(provider)) {
      "#imageUploader [action]" #> S.uri &
        "#imageUploader" #> {
          "name=params [value]" #> transloadItParams
        }
    } else ".add *" #> ""

  }

  def noTags = ".tag" #> ""
  def tagsNotAllowed = "* *" #> ""

  def deleteTag(tag: TransactionTagJson) = {
    SHtml.a(() => {
      tag.id match {
        case Some(id) => {
          val worked = ObpAPI.deleteTag(urlParams.bankId, urlParams.accountId, urlParams.viewId,
            urlParams.transactionId, id)
            if(!worked) logger.warn("Deleting tag with id " + id + " failed")
        }
        case _ => logger.warn("Tried to delete a tag without an id")
      }
      Hide(tag.id.getOrElse(""))
    }, Text("x"), ("title", "Remove the tag"))
  }
  
  def showTags = {
    
    val tagJsons = transactionJson.tagJsons

    def showTags(tags : List[TransactionTagJson]) = {
      def orderByDateDescending = 
        (tag1: TransactionTagJson, tag2: TransactionTagJson) =>
          tag1.date.getOrElse(now).before(tag2.date.getOrElse(now))
      
      ".tagsContainer" #> {
        "#noTags" #> "" &
          ".tag" #> tags.sortWith(orderByDateDescending).map(getTagSelector)
      }
    }
    
    tagJsons match {
      case Some(tJsons) => {
        if (tJsons.isEmpty) noTags
        else showTags(tJsons)
      }
      case _ => tagsNotAllowed
    }
    
  }
  
  def getTagSelector (tag: TransactionTagJson) = {
		 ".tagID [id]" #> tag.id.getOrElse("") &
		 ".tagValue" #> tag.value.getOrElse("") &
         ".deleteTag" #> deleteTag(tag)
  }

  def addTag(xhtml: NodeSeq): NodeSeq = {

    var tagValues : List[String] = Nil
    
    def newTagsXml(newTags: List[TransactionTagJson]): Box[NodeSeq] = {
      Templates(List("templates-hidden", "_tag")).map {
        ".tag" #> newTags.map { getTagSelector }
      }
    }
    
    def addTagSelector = {
      SHtml.ajaxForm(
        SHtml.text("",
          tags => {
            val tagVals = tags.split(" ").toList.filter(tag => !tag.isEmpty)
            tagValues = tagVals
          },
          ("placeholder", "Add tags seperated by spaces"),
          ("id", "addTagInput"),
          ("size", "30")) ++
          SHtml.ajaxSubmit(
            "Tag",
            () => {
              val newTags = ObpAPI.addTags(urlParams.bankId, urlParams.accountId, urlParams.viewId,
                  urlParams.transactionId, tagValues)
              val newXml = newTagsXml(newTags)
              //TODO: update the page
              val content = Str("")
              SetValById("addTagInput",content)&
              SetHtml("noTags",NodeSeq.Empty) &
              AppendHtml("tags_list", newXml.getOrElse(NodeSeq.Empty))
            },
            ("id", "submitTag")))
    }
    
    //TODO: Support multiple providers
    if(OAuthClient.loggedInAt.contains(OAuthClient.defaultProvider)) {
      addTagSelector
    } else {
      (".add" #> "You need to login before you can add tags").apply(xhtml)
    }
  }

  def noComments = ".comment" #> ""
  def commentsNotAllowed = "* *" #> ""

  def showComments = {
    val commentJsons = transactionJson.commentJsons

    def showComments(comments: List[TransactionCommentJson]) = {

      def orderByDateDescending =
        (comment1: TransactionCommentJson, comment2: TransactionCommentJson) =>
          comment1.date.getOrElse(now).before(comment2.date.getOrElse(now))

      ".commentsContainer" #> {
        "#noComments" #> "" &
          ".comment" #> comments.sortWith(orderByDateDescending).zipWithIndex.map {
            case (commentJson, position) => {

              def commentDate : CssSel = {
                commentJson.date.map(d => {
                  ".commentDate *" #> commentDateFormat.format(d)
                }) getOrElse
                  ".commentDate" #> ""
              }

              def userInfo : CssSel = {
                commentJson.user.map(u => {
                  ".userInfo *" #> {
                    " -- " + u.display_name.getOrElse("")
                  }
                }) getOrElse
                  ".userInfo" #> ""
              }
              
              val displayPosition = position + 1
              val commentId = "comment_" + displayPosition
              
              ".text *" #> commentJson.value.getOrElse("") &
              ".commentLink * " #> { "#" + displayPosition } &
              ".commentLink [id]" #> commentId &
              ".commentLink [href]" #> { "#" + commentId } &
              commentDate &
              userInfo
            }
          }
      }
    }
    
    commentJsons match {
      case Some(cJsons) => {
        if (cJsons.isEmpty) noComments
        else showComments(cJsons)
      }
      case _ => commentsNotAllowed
    }

  }

  var commentsListSize = transaction.metadata match {
    case Some(metadata) => metadata.comments match {
      case Some(comments) => comments.size
      case _ =>  0
    }
    case _ => 0
  }

  def addComment(xhtml: NodeSeq) : NodeSeq = {
    User.currentUser match {
      case Full(user) =>
        transaction.metadata match {
          case Some(metadata) =>
            metadata.addComment match {
              case Some(addComment) => {
                var commentText = ""
                var commentDate = new Date
                SHtml.ajaxForm(
                  SHtml.textarea(
                    "",
                    comment => {
                      commentText = comment
                      commentDate = new Date
                      addComment(user.id_, view.id, comment,commentDate)
                    },
                    ("rows","4"),("cols","50"),
                    ("id","addCommentTextArea"),
                    ("placeholder","add a comment here")
                  ) ++
                  SHtml.ajaxSubmit("add a comment",() => {
                    val commentXml = Templates(List("templates-hidden","_comment")).map({
                      commentsListSize = commentsListSize + 1
                      val commentId="comment_"+commentsListSize.toString
                      ".commentLink * " #>{"#"+ commentsListSize} &
                      ".commentLink [id]"#>commentId &
                      ".commentLink [href]" #>{"#"+ commentId} &
                      ".text *" #> {commentText} &
                      ".commentDate *" #> {commentDateFormat.format(commentDate)} &
                      ".userInfo *" #> { " -- " + user.theFirstName + " "+ user.theLastName}
                    })
                    val content = Str("")
                    SetValById("addCommentTextArea",content)&
                    SetHtml("noComments",NodeSeq.Empty) &
                    AppendHtml("comment_list",commentXml.getOrElse(NodeSeq.Empty))
                  },("id","submitComment"))
                )
              }
              case _ => (".add" #> "You cannot comment transactions on this view").apply(xhtml)
            }
          case _ => (".add" #> "You Cannot comment transactions on this view").apply(xhtml)
        }
      case _ => (".add" #> "You need to login before you can submit a comment").apply(xhtml)
    }
  }
}