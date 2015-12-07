/**
Open Bank Project - Transparency / Social Finance Web Application
Copyright (C) 2011 - 2015, TESOBE  Ltd.

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
TESOBE Ltd.
Osloer Str. 16/17
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

import net.liftweb.http.{PaginatorSnippet, StatefulSnippet}
import java.text.SimpleDateFormat
import net.liftweb.http._
import java.util.Calendar
import xml.NodeSeq
import net.liftweb.util.Helpers._
import net.liftweb.util._
import scala.xml.Text
import net.liftweb.common.{Box, Failure, Empty, Full}
import java.util.Date
import net.liftweb.http.js.JsCmds.Noop
import java.util.Currency
import code.lib.ObpJson._
import code.lib._
import net.liftweb.json.JsonDSL._
import code.util.Helper._

case class TransactionsListURLParams(bankId: String, accountId: String, viewId: String)

class OBPTransactionSnippet (params : (TransactionsJson, AccountJson, TransactionsListURLParams)){

  val transactionsJson = params._1
  val accountJson = params._2
  val transactionsURLParams = params._3
  val NOOP_SELECTOR = "#i_am_an_id_that_should_never_exist" #> ""
  val FORBIDDEN = "---"

  val currencySymbol = {
    transactionsJson.transactions match {
      case None | Some(Nil) => ""
      case Some(x :: xs) => {
        (for {
          details <- x.details
          value <- details.value
          currencyCode <- value.currency
          currency <- tryo{Currency.getInstance(currencyCode)}
          symbol <- tryo{currency.getSymbol(S.locale)}
        } yield symbol).getOrElse("").replace("GBP","£").replace("EUR","€").replace("USD","$") // TODO: Better way to get short symbol?
      }
    }
  }

  def individualApiTransaction(transaction: TransactionJson): CssSel = {

    def otherPartyInfo: CssSel = {

      def info(oAcc : OtherAccountJson): CssSel = {

        def moreInfo(metadata : OtherAccountMetadataJson) = {
          
          def moreInfoBlank = 
            ".other_account_more_info" #> NodeSeq.Empty &
            ".other_account_more_info_br" #> NodeSeq.Empty
          
          metadata.more_info.map(".other_account_more_info *" #> _).getOrElse(moreInfoBlank)
        }
        
        def logo(metadata : OtherAccountMetadataJson) = {
          metadata.image_URL.map(".other_account_logo_img [src]" #> _).getOrElse(NOOP_SELECTOR)
        }

        def website(metadata : OtherAccountMetadataJson) = {
          def websiteBlank = 
            ".other_acc_link" #> NodeSeq.Empty & //If there is no link to display, don't render the <a> element
            ".other_acc_link_br" #> NodeSeq.Empty
          
          metadata.URL.map(".other_acc_link [href]" #> _).getOrElse(websiteBlank)
        }

        def openCorporates(metadata : OtherAccountMetadataJson) = {
          def openCorporatesBlank =
            ".open_corporates_link" #> NodeSeq.Empty

          metadata.open_corporates_URL.map(".other_acc_link [href]" #> _).getOrElse(openCorporatesBlank)
        }
        
        def aliasSelector = {
          val hasManagementAccess = {
            val availableViews = accountJson.views_available.toList.flatten
            availableViews.exists(view => view.id == Some("owner"))
          }

          def aliasInfo = {

            val name = oAcc.holder.flatMap(_.name)
            val isAlias = oAcc.holder.flatMap(_.is_alias).getOrElse(true)
            
            val privateAlias = oAcc.metadata.flatMap(_.private_alias)
            val publicAlias = oAcc.metadata.flatMap(_.public_alias)
            
            if(isAlias) {
              if(publicAlias == name) {
                ".alias_indicator [class+]" #> "alias_indicator_public" &
                ".alias_indicator *" #> "(Alias)"
              } else if (privateAlias == name) {
                ".alias_indicator [class+]" #> "alias_indicator_private" &
                ".alias_indicator *" #> "(Alias)"
              } else ".alias_indicator" #> ""
            } else ".alias_indicator" #> ""
          }

          def aliasName = {
            
            val otherAccountLink = "management#" + oAcc.id.getOrElse("")
            
            if (hasManagementAccess) {
              ".otherAccountLinkForName [href]" #> otherAccountLink &
              ".otherAccountLinkForName *" #> oAcc.holder.flatMap(_.name).getOrElse(FORBIDDEN) &
              ".otherAccountLinkForAlias [href]" #> otherAccountLink
            } else {
              ".the_name *" #> oAcc.holder.flatMap(_.name).getOrElse(FORBIDDEN)
            }
          }
          
          aliasInfo &
          aliasName
        }
        
        aliasSelector &
        (oAcc.metadata match {
          case None => ".extra *" #> NodeSeq.Empty
          case Some(meta) => {
            moreInfo(meta) &
            logo(meta) &
            website(meta) &
            openCorporates(meta)
          }
        })
      }
      
      transaction.other_account match {
        case None => ".the_name *" #> NodeSeq.Empty & ".extra *" #> NodeSeq.Empty
        case Some(oAcc) => info(oAcc)
      }
      
    }

    def transactionInformation: CssSel = {

      def description = {
        ".description *" #> {
          val description = transaction.details.flatMap(_.label) match {
            case Some(a) => a
            case _ => FORBIDDEN // TODO Different symbol for forbidden / empty?
          }
          description
        }
      }



      def amount = {

        ".amount *" #> {
          val amount = transaction.details.flatMap(_.value.flatMap(_.amount)) match {
            case Some(a) => a.stripPrefix("-")
            case _ => ""
          }
          currencySymbol + " " + amount
        } &
        ".amount [class+]" #> {
          isPositiveAmount match {
            case Full(isPos) => if (isPos) "green--color" else "orange--color"
            case _ => ""
          }
        } &
          ".money_direction [src]" #> {
            isPositiveAmount match {
              case Full(isPos) => if (isPos) "/media/images/money-in-icon.png" else "/media/images/money-out-icon.png"
              case _ => ""
            }
          }

      }
      
      def narrative = {

        val narrativeValue = transaction.metadata.flatMap(_.narrative).getOrElse("")
        // TODO use v1.4.0
        val narrativeUrl = "/v1.2/banks/" + transactionsURLParams.bankId + "/accounts/" + transactionsURLParams.accountId + "/" + transactionsURLParams.viewId +
          "/transactions/" + transaction.id.getOrElse("") + "/metadata/narrative"
        
        var newNarrativeValue = narrativeValue
        var exists = !newNarrativeValue.isEmpty
        
        def json() = ("narrative" -> newNarrativeValue)

        def saveValue() = {
          if (newNarrativeValue.isEmpty) {
            //Send a delete
            ObpDeleteBoolean(narrativeUrl)
            exists = false
          } else {
            if (exists) {
              ObpPut(narrativeUrl, json())
            } else {
              ObpPost(narrativeUrl, json())
              exists = true
            }
          }
      }
        
        def apiEditableNarrative = {
          CustomEditable.editable(
            newNarrativeValue,
            SHtml.text(newNarrativeValue, newNarrativeValue = _),
            () => {
              saveValue()
              Noop
            },
            () => {},
            "Narrative",
          false)
        }
        
        //TODO: Get this from the api
        def canEditNarrative = transactionsURLParams.viewId == "owner"

        ".narrative *" #> {
          if (canEditNarrative) apiEditableNarrative
          else Text(narrativeValue)
        }
      }
      
      /**
       * @return a full box containing the answer if everything went well, or something else if the
       * information was not available/badly formatted
       */
      def isPositiveAmount : Box[Boolean] = {
        for {
            details <- transaction.details
            value <- details.value
            amount <- value.amount
            amountAsDouble <- tryo{amount.toDouble}
         } yield (amountAsDouble > 0)
      }

      def symbol = {
        ".symbol *" #> {
          isPositiveAmount match {
            case Full(isPos) => if (isPos) "+" else "-"
            case _ => ""
          }
        }
      }

      def transactionInOrOut = {
        ".out [class]" #> {
          isPositiveAmount match {
            case Full(isPos) => if (isPos) "in" else "out"
            case _ => ""
          }
        }
      }
      
      def comments = {
        val commentSelector = for {
          metadata <- transaction.metadata
          comments <- metadata.comments
        } yield {
          ".comments_ext [href]" #> { "transactions/" + transaction.id.getOrElse("") + "/" + transactionsURLParams.viewId} &
          ".comment *" #> comments.size.toString
        }
        
        commentSelector getOrElse {".comments *" #> NodeSeq.Empty}
      }
      
      def images = {
        
        def hideImages = ".transaction_images *" #> ""
        
        val imagesSelector = for {
          metadata <- transaction.metadata
          images <- metadata.images
        } yield {
          if(images.nonEmpty) {
            ".transaction_image [src]" #> images(0).URL.getOrElse("/notfound.png") &
            {
              if(images(0).label.isDefined) "transaction_image [alt]" #> images(0).label
              else NOOP_SELECTOR
            }
          } else hideImages
        }
        
        imagesSelector getOrElse hideImages
      }
      
      def tags = {
        
        def hideTags = ".tags *" #> ""
        
        val tagSelector = for {
          metadata <- transaction.metadata
          tags <- metadata.tags
        } yield {
         if(tags.nonEmpty) {
           //only first 3 elements of the list should be displayed
           val tags3 = tags.splitAt(3)._1
           ".tags *" #>  {
                  ".tag *" #>  tags3.map(tag => {
                  ".tagContent *" #> tag.value
                  })}
         } else hideTags
        }
        
        tagSelector getOrElse hideTags
      }
      
      amount &
      description &
      narrative &
      symbol &
      transactionInOrOut &
      comments &
      images &
      tags
    }

    transactionInformation &
    otherPartyInfo
  }


  /*
  Used in the display of the transactions list
  e.g. http://localhost:8080/banks/bnpp-fr2/accounts/1137869186/public
  Also used for dashboard
   */
  def displayAll = {
    val groupedApiTransactions = groupByDate(transactionsJson.transactions.getOrElse(Nil))
    ".account_grouped_by_date *" #> groupedApiTransactions.map(daySummary) &  // The previous CSS selector was "* *"
    ".account_title *" #> getAccountTitle(accountJson) &
    ".view_id *" #> transactionsURLParams.viewId
  }

 
  
  def hasSameDate(t1: TransactionJson, t2: TransactionJson) : Boolean = {
    
    def getDate(t : TransactionJson) : Date = (for {
      details <- t.details
      finishDate <- details.completed
    } yield finishDate).getOrElse(now)
    
    val date1 = getDate(t1)
    val date2 = getDate(t2)
    
    val cal1 = Calendar.getInstance();
    val cal2 = Calendar.getInstance();
    cal1.setTime(date1);
    cal2.setTime(date2);

    //True if the two dates fall on the same day of the same year
    cal1.get(Calendar.YEAR) == cal2.get(Calendar.YEAR) &&
                  cal1.get(Calendar.DAY_OF_YEAR) == cal2.get(Calendar.DAY_OF_YEAR);
  }


/*
Used in transactions list

   */

  def groupByDate(list : List[TransactionJson]) : List[List[TransactionJson]] = {
    list match {
      case Nil => Nil
      case h :: Nil => List(list)
      case h :: t => {
        //transactions that are identical to the head of the list
        val matches = list.filter(hasSameDate(h, _))
        List(matches) ++ groupByDate(list diff matches)
      }
    }
  }
  
  def daySummary(transactionsForDay: List[TransactionJson]) = {
    val aTransaction = transactionsForDay.last
    val date = aTransaction.details.flatMap(_.completed) match {
      case Some(d) => (new SimpleDateFormat("MMMM dd, yyyy")).format(d)
      case _ => ""
    }
    val amount : String = (for {
      details <- aTransaction.details
      newBalance <- details.new_balance
      amt <- newBalance.amount
    } yield amt).getOrElse("")

    val isPositiveSumAmount : Box[Boolean] = {
      val amountAsDouble = amount.toDouble
      Full(amountAsDouble > 0)
    }
    
    ".date *" #> date &
    ".balance_number *" #> {currencySymbol + "" + amount} &
    ".transaction_row *" #> transactionsForDay.map(individualApiTransaction) &
    ".balance--cell [class+]" #> {
      isPositiveSumAmount match {
        case Full(isPos) => if (isPos) "green--color" else "red--color"
        case _ => ""
      }
    }
  }

  def accountDetails = {

    //TODO: We don't have access to this information via the api (yet)
    def lastUpdated = {
      "#lastUpdate *" #> ""
    }

    def accountLabel = {
      val accountTitle = getAccountTitle(accountJson)
      //"#accountShortDescription *" #> accountTitle &
      ".seb *" #> "simon" // accountTitle
    }

    /*    LocalStorage.getAccount(url(2), url(4)) match {
                  case Full(account) => "#lastUpdate *"#> {
                    val dateFormat = new SimpleDateFormat("kk:mm EEE MMM dd yyyy")
                     if(!account.lastUpdate.asString.contains("Empty"))
                       "Last updated : " + dateFormat.format(account.lastUpdate.get)
                     else
                       "Not yet updated"
                  }
                  case _ =>  NOOP_SELECTOR
                }
              }
    }*/

    accountLabel //&//lastUpdated
  }
  
  def hideSocialWidgets = {
    if(transactionsURLParams.viewId != "Public") "#socialButtons *" #> NodeSeq.Empty
    else NOOP_SELECTOR
  }
}
