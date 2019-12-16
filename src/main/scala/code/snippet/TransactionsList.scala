/**
Open Bank Project - Sofi Web Application
Copyright (C) 2011 - 2016, TESOBE  Ltd.

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

import java.text.SimpleDateFormat
import java.util.{Calendar, Currency, Date}

import code.Constant._
import code.lib.ObpJson._
import code.lib._
import code.util.Helper._
import net.liftweb.common.{Box, Empty, Full}
import net.liftweb.http._
import net.liftweb.http.js.JsCmds.Noop
import net.liftweb.json.JsonDSL._
import net.liftweb.util.Helpers._
import net.liftweb.util.{Props, _}

import scala.xml.{NodeSeq, Text}

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

  val hasManagementAccess = {
    val availableViews = accountJson.views_available.toList.flatten
    availableViews.exists(view => view.id == Some(CUSTOM_OWNER_VIEW_ID))
  }


  def individualApiTransaction(transaction: TransactionJson): CssSel = {
    val transactionURI = "transactions/" + transaction.id.getOrElse("") + "/" + transactionsURLParams.viewId
    
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

            // This is where we set the counterparty (aka other account) display value
            // and id (used for link to its metadata)

            val otherAccountLink = "management#" + oAcc.id.getOrElse("")

            // Only show the edit link if allowed

            if (hasManagementAccess) {
              ".otherAccountLinkForName [href]" #> otherAccountLink &
              ".otherAccountLinkForName *" #> oAcc.holder.flatMap(_.name).getOrElse(FORBIDDEN) &
              ".otherAccountLinkForAlias [href]" #> otherAccountLink
            } else {
              ".otherAccountLinkForName [href]" #> transactionURI &
              ".otherAccountLinkForName *" #> oAcc.holder.flatMap(_.name).getOrElse(FORBIDDEN)
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
          val descriptionDisplayValue = transaction.details.flatMap(_.description) match {
            case Some(a) => a
            case _ => FORBIDDEN // TODO Different symbol for forbidden / empty?
          }

          val countpartyDisplayValue = transaction.other_account.map(_.holder.flatMap(_.name)) match {
            case Some(a) => a.getOrElse(FORBIDDEN)
            case _ => FORBIDDEN // FORBIDDEN might just mean empty here.
          }

          // Only show the Description if its not the same as the Counterparty.
          // It's not really nice to rely on FORBIDDEN in equality tests, because we will use it where it doesn't really mean FORBIDDEN
          if (countpartyDisplayValue == descriptionDisplayValue) "" else descriptionDisplayValue
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
        }
      }
      

      def icon = {
          ".icon [src]" #> {
            isPositiveAmount match {
              case Full(isPos) => if (isPos) "/media/images/home-table-enter-icon.png" else "/media/images/home-table-exit-icon.png"
              case _ => ""
            }
          } &
          ".icon [alt]" #> {
            isPositiveAmount match {
              case Full(isPos) => if (isPos) "Enter icon" else "Exit icon"
              case _ => ""
            }
          }
      }


      def narrative = {

        val narrativeValue = transaction.metadata.flatMap(_.narrative).getOrElse("")
        // TODO use v1.4.0
        val narrativeUrl = "/v1.2.1/banks/" + transactionsURLParams.bankId + "/accounts/" + transactionsURLParams.accountId + "/" + transactionsURLParams.viewId +
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
            "",
          false)
        }
        
        //TODO: Get this from the api
        def canEditNarrative = transactionsURLParams.viewId == CUSTOM_OWNER_VIEW_ID

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

    
      def comments = {
        val commentSelector = for {
          metadata <- transaction.metadata
          comments <- metadata.comments
        } yield {
          ".comments_page [href]" #> { transactionURI } &
          ".comments_bloc [href]" #> { transactionURI + "#commentsBloc"} &
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
      icon &
      description &
      narrative &
      comments &
      images &
      tags
    }

    transactionInformation &
    otherPartyInfo
  }

  class PersistentDouble(initialValue: Double) {
    var _total:Double = initialValue

    def set(total: Double) =
      _total = total

    def get =
      _total
  }

  /*
  Used in the display of the transactions list
  e.g. http://localhost:8080/banks/bnpp-fr2/accounts/1137869186/public
   */
  def displayAll = {
    accountJson.balance match {
      case Some(balance) if balance.amount.isDefined =>
        val total = new PersistentDouble(accountJson.balance.get.amount.get.toDouble)
        val groupedApiTransactions = groupByDate(sortByDate(transactionsJson.transactions.getOrElse(Nil)))
        ".account_grouped_by_date *" #> groupedApiTransactions.map(daySummary(_, total)) // The previous CSS selector was "* *"
      case _ =>
        val total = new PersistentDouble(0.toDouble)
        val groupedApiTransactions = Nil
        ".account_grouped_by_date *" #> groupedApiTransactions.map(daySummary(_, total)) // The previous CSS selector was "* *"
    } 
    
  }


  def displayForDashboard = {
    val total = new PersistentDouble(accountJson.balance.get.amount.get.toDouble)
    val groupedApiTransactions = groupByDate(sortByDate(transactionsJson.transactions.getOrElse(Nil)))
    ".account_grouped_by_date *" #> groupedApiTransactions.map(daySummary(_, total)) &  // The previous CSS selector was "* *"
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
  def sortByDate(list : List[TransactionJson]) : List[TransactionJson] = {
    list match {
      case Nil => Nil
      case h :: Nil => list
      case h :: t => {
        list.sortBy(_.details.get.completed).reverse
      }
    }
  }

  def groupByDate(list : List[TransactionJson]) : List[List[TransactionJson]] = {
    list match {
      case Nil => Nil
      case h :: Nil => List(list)
      case h :: t => {
        //transactions that are identical to the head of the list
        // TODO sort by date or check API sort (should be completed date reverse)
        // .sortBy(_.details.get.completed)
        val matches = list.filter(hasSameDate(h, _))
        List(matches) ++ groupByDate(list diff matches)
      }
    }
  }

  def daySummary(transactionsForDay: List[TransactionJson], _total: PersistentDouble ) = {
    val aTransaction = transactionsForDay.sortBy(_.details.get.completed).last
    val date = aTransaction.details.flatMap(_.completed) match {
      case Some(d) => (new SimpleDateFormat("MMMM dd, yyyy")).format(d)
      case _ => ""
    }
    val amount : String = (for {
      details <- aTransaction.details
      newBalance <- details.new_balance
      amt <- newBalance.amount
    } yield {
      // Workaround when new_balance is not included in transaction details
      if (Props.getBool("calculateTransactionNewBalance", false)) {
        val total = _total.get
        _total.set(total - transactionsForDay.foldLeft(0.0)(_ + _.details.get.value.get.amount.get.toDouble))
        total.toString
      }
      else
        amt
    }).getOrElse("")

    // If the view doesn't allow the amount to be seen we could get an empty string here
    // If amount is not a number return None
    val isPositiveSumAmount : Box[Boolean] = {
      try {
        Full(amount.toDouble > 0)
      } catch {
        case e: Exception => None
      }
    }
    
    ".date *" #> date &
    ".balance_number *" #> {currencySymbol + "" + amount} &
    ".transaction_row *" #> transactionsForDay.map(individualApiTransaction) &
    ".balance--cell [class+]" #> {
      isPositiveSumAmount match {
        case Full(isPos) => if (isPos) "green--color" else "orange--color"
        case _ => ""
      }
    } &
    ".balance--cell *" #> {
      isPositiveSumAmount match {
        case Empty => "" // Hide the balance string if it is None (i.e. the view does not allow it to be seen)
        case _ => "Balance"
      }
    }
  }


  def accountTitle = ".account-title *" #> getAccountTitle(accountJson)


  def hideSocialWidgets = {
    if(transactionsURLParams.viewId != "Public") "#socialButtons *" #> NodeSeq.Empty
    else NOOP_SELECTOR
  }
}
