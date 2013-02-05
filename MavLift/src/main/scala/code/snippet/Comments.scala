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
import code.model.dataAccess.OBPEnvelope
import code.model.dataAccess.OBPAccount.{APublicAlias,APrivateAlias}
import net.liftweb.common.Full
import scala.xml.NodeSeq
import net.liftweb.http.SHtml
import net.liftweb.common.Box
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JE.JsRaw
import net.liftweb.http.js.JsCmds.RedirectTo
import net.liftweb.http.SessionVar
import scala.xml.Text
import net.liftweb.json.JsonAST.JObject
import net.liftweb.json.JsonAST.JField
import net.liftweb.json.JsonAST.JString
import net.liftweb.json.JsonAST.JArray
import net.liftweb.http.StringField
import java.util.Date
import java.text.SimpleDateFormat
import code.model.dataAccess.{OBPAccount,OBPUser}
import net.liftweb.common.Loggable
import code.model.dataAccess.Account
import code.model.traits.{ModeratedTransaction,PublicAlias,PrivateAlias,NoAlias,Comment, View, Tag}
import java.util.Currency
import net.liftweb.http.js.jquery.JqJsCmds.{AppendHtml,Hide}
import net.liftweb.http.js.JsCmds.{SetHtml,SetValById}
import net.liftweb.http.js.JE.Str 
import net.liftweb.http.js.JsCmds.Alert

/**
 * This whole class is a rather hastily put together mess
 */
class Comments(transactionAndView : (ModeratedTransaction,View)) extends Loggable{
  val transaction = transactionAndView._1
  val view = transactionAndView._2
  val commentDateFormat = new SimpleDateFormat("kk:mm:ss EEE MMM dd yyyy")
  val NOOP_SELECTOR = "#i_am_an_id_that_should_never_exist" #> ""  
  
  def commentPageTitle(xhtml: NodeSeq): NodeSeq = {
    val FORBIDDEN = "---"
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
        val amount = transaction.amount match {
          case Some(amount) => amount.toString
          case _ => FORBIDDEN
        }
        theCurrency = transaction.currency match {
          case Some(currencyISOCode) => tryo{
                    Currency.getInstance(currencyISOCode)
                  } match {
                    case Full(currency) => currency.getSymbol(S.locale)
                    case _ => FORBIDDEN
                  }
          case _ => FORBIDDEN
        } 
        {amount + " " + theCurrency}
      } &
      ".other_account_holder *" #> {
        transaction.otherBankAccount match {
          case Some(otherBankaccount) =>{
            ".the_name" #> otherBankaccount.label.display &
            {otherBankaccount.label.aliasType match {
                case PublicAlias => ".alias_indicator [class+]" #> "alias_indicator_public" &
                    ".alias_indicator *" #> "(Alias)"
                case PrivateAlias => ".alias_indicator [class+]" #> "alias_indicator_private" &
                    ".alias_indicator *" #> "(Alias)"
                case _ => NOOP_SELECTOR
            }} 
          }
          case _ => "* *" #> FORBIDDEN
        }
      } &
      ".date_cleared *" #> {
        transaction.finishDate match {
          case Some(date) => formatDate(Full(date))
          case _ => FORBIDDEN 
        }
      } &
      ".label *" #> {
            transaction.label.getOrElse(FORBIDDEN)
      } &
      ".new_balance *" #> {
            transaction.balance + " " + theCurrency
      }
    ).apply(xhtml)
  }
  def noTags = ".tag" #> ""
  
  def showTags = 
    transaction.metadata match {
      case Some(metadata) => metadata.tags match {
        case Some(tags) => 
          if(tags.isEmpty)
            noTags
          else
            ".tagsContainer" #>
            { 
              def orderByDateDescending = (tag1 : Tag, tag2 : Tag) =>
                tag1.datePosted.before(tag2.datePosted)
              "#noTags" #> "" &
              ".tag" #>
                tags.sortWith(orderByDateDescending).map(tag => {
                  ".tagID [id]" #> tag.id_ &
                  ".tagValue" #> tag.value & 
                  ".deleteTag" #> 
                    SHtml.a(
                      () => {
                        metadata.deleteTag.map(t => t(tag.id_))
                        Hide(tag.id_)
                      },
                      Text("x"),
                      ("title","Remove the tag")
                    )                  
                })
            }
        case _ => noTags
      }
      case _ => noTags
    }
  def addTag(xhtml: NodeSeq) : NodeSeq = 
    OBPUser.currentUser match {
      case Full(user) =>     
        transaction.metadata match {
          case Some(metadata) =>
            metadata.addTag match {
              case Some(addTag) => {
                var tagValues : List[String] = Nil
                var tagDate = new Date
                var tagIds : List[String] = List()
                SHtml.ajaxForm(
                  SHtml.text(
                    "",
                    tags => {
                      val tagsList = tags.split(" ").toList.filter(tag => !tag.isEmpty)
                      tagValues = tagsList
                      tagDate = new Date
                      tagIds = tagsList.map(addTag(user.id, view.id, _ ,tagDate))
                    },
                    ("placeholder","Add tags seperated by spaces"),
                    ("id","addTagInput"),
                    ("size","30") 
                  ) ++
                  SHtml.ajaxSubmit(
                    "Tag",
                    () => {
                      val tagXml = Templates(List("templates-hidden","_tag")).map({ 
                        ".tag" #> {
                          tagValues.zipWithIndex.map(t => {
                            ".tagID [id]" #> tagIds(t._2) &
                            ".tagValue" #> t._1 &
                            ".deleteTag" #> 
                              SHtml.a(
                                () => {
                                  metadata.deleteTag match {
                                    case Some(del) => {
                                      del(tagIds(t._2))
                                      Hide(tagIds(t._2))
                                    }
                                    case _ => Alert("deleting tags is not allowed in this view")
                                  }
                                },
                                Text("x"),
                                ("title","Remove the tag")
                              )  
                          })
                        }

                      })
                      val content = Str("")
                      SetValById("addTagInput",content)&
                      SetHtml("noTags",NodeSeq.Empty) &
                      AppendHtml("tags_list",tagXml.getOrElse(NodeSeq.Empty))
                    },
                    ("id","submitTag")
                  )
                )
              }
              case _ => (".add" #> "You cannot add tags to transactions on this view").apply(xhtml)
            }
          case _ => (".add" #> "You cannot add tags to transactions on this view").apply(xhtml)
        }
      case _ => (".add" #> "You need to login before you can add tags").apply(xhtml) 
    }   

  def showComments = 
    transaction.metadata match {
      case Some(metadata)  => 
        metadata.comments match {
          case Some(comments) => 
            if(comments.size==0)
              ".comment" #> ""
            else
            ".commentsContainer" #>
            { 
              def orderByDateDescending = (comment1 : Comment, comment2 : Comment) =>
                comment1.datePosted.before(comment2.datePosted)
              "#noComments" #> "" &
              ".comment" #>
                comments.sortWith(orderByDateDescending).zipWithIndex.map(comment => {
                  val commentId="comment_"+{comment._2 + 1 }
                  ".commentLink * " #>{"#"+ {comment._2 + 1}} &
                  ".commentLink [id]"#>commentId &                  
                  ".commentLink [href]" #>{"#"+ commentId} & 
                  ".text *" #> {comment._1.text} &
                  ".commentDate *" #> {commentDateFormat.format(comment._1.datePosted)} &
                  ".userInfo *" #> {
                      comment._1.postedBy match {
                        case Full(user) => {" -- " + user.theFistName + " "+ user.theLastName}
                        case _ => "-- user not found" 
                      }
                  }
                })
            }
          case _ => ".comment" #> "" 
        }
      case _ => ".comment" #> ""
    }
  
  var commentsListSize = transaction.metadata match {
    case Some(metadata) => metadata.comments match {
      case Some(comments) => comments.size
      case _ =>  0
    }
    case _ => 0 
  }
  
  def addComment(xhtml: NodeSeq) : NodeSeq = {
    OBPUser.currentUser match {
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
                      addComment(user.id, view.id, comment,commentDate)
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
                      ".userInfo *" #> { " -- " + user.theFistName + " "+ user.theLastName}
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