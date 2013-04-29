package code.lib

import net.liftweb.json._
import net.liftweb.json.JObject
import net.liftweb.json.JsonDSL._
import net.liftweb.common.Box
import net.liftweb.common.Full
import net.liftweb.json.JsonAST.JValue
import net.liftweb.json.JsonAST.JInt
import net.liftweb.json.JDouble
import net.liftweb.common.Empty
import java.net.URL
import org.apache.http.client.HttpClient
import java.net.HttpURLConnection
import net.liftweb.common.Failure
import java.io.BufferedReader
import net.liftweb.util.Helpers._
import java.io.InputStreamReader
import java.util.Date

object ObpGet {
  //Ah, dispatch does have oauth support. It would be nicer to use dispatch! -E.S.
  def apply(apiPath: String): Box[JValue] = {
    tryo {
      val credentials = OAuthClient.getOrCreateCredential(OAuthClient.defaultProvider) //TODO: Support multiple providers
      val apiUrl = credentials.provider.apiBaseUrl
      val url = new URL(apiUrl + apiPath)
      //bleh
      val request = url.openConnection().asInstanceOf[HttpURLConnection] //blagh!
      request.setRequestMethod("GET")
      request.setRequestProperty("Content-Type", "application/json")
      request.setRequestProperty("Accept", "application/json")

      val consumer = credentials.consumer
      consumer.sign(request)
      request.connect()

      val status = request.getResponseCode()

      status match {
        case 200 => {
          //bleh
          val reader = new BufferedReader(new InputStreamReader(request.getInputStream()))
          val builder = new StringBuilder()
          var line = ""
          def readLines() {
            line = reader.readLine()
            if (line != null) {
              builder.append(line + "\n")
              readLines()
            }
          }
          readLines()
          reader.close();
          val result = builder.toString();
          parse(result)
        }
        case code => {
          throw new Exception("Bad response code from server: " + code) //bleh -> converts to Failure due to the tryo
        }
      }
    }
  }
}

object ObpJson {
  import net.liftweb.json._
  implicit val formats = DefaultFormats
  case class BanksJson(banks : Option[List[BankJson]])
  case class BankJson(id: Option[String], 
		  		  short_name: Option[String],
		  		  full_name: Option[String],
		  		  logo: Option[String],
		  		  website: Option[String])
		  		  
  case class UserJson(user_id: Option[String],
		  				  user_provider: Option[String],
		  				  display_name: Option[String])
  
  case class AccountBalanceJson(currency: Option[String],
		  					amount: Option[String])		  	
	
  case class ViewJson(id: Option[String],
		  		  short_name: Option[String],
		  		  description: Option[String],
		  		  is_public: Option[Boolean])
		  		  
  case class AccountJson(id: Option[String],
		  			 number: Option[String],
		  			 owners: Option[List[UserJson]],
		  			 `type`: Option[String],
		  			 balance: Option[AccountBalanceJson],
		  			 IBAN : Option[String],
		  			 views_available: Option[List[ViewJson]])
		  			 
  case class BarebonesAccountJson(id: Option[String],
		  						  label: Option[String],
		  						  views_available: Option[List[ViewJson]])
		  						  
  case class HolderJson(name: Option[String],
		  				is_alias : Option[Boolean])
		  				
  //TODO: Can this go with BankJson?
  case class LightBankJson(national_identifier: Option[String],
		  			  	   name: Option[String])
  
  case class ThisAccountJson(holders: Option[List[HolderJson]],
		  					 number: Option[String],
		  					 kind: Option[String],
		  					 IBAN: Option[String],
		  					 bank: Option[LightBankJson])
  
  case class LocationJson(latitude: Option[Double],
		  						   longitude: Option[Double],
		  						   date: Option[Date], //TODO: Check if the default date formatter is okay
		  						   user: Option[UserJson])
  
  case class OtherAccountMetadataJson(public_alias: Option[String],
		  							  private_alias: Option[String],
		  							  more_info: Option[String],
		  							  URL: Option[String],
		  							  image_URL: Option[String],
		  							  open_corporates_URL: Option[String],
		  							  corporate_location: Option[LocationJson],
		  							  physical_location: Option[LocationJson])		  					 
		  					 
  //TODO: Why can't an other account have more than one holder?	  					 
  case class OtherAccountJson(holder: Option[HolderJson],
		  					  number: Option[String],
		  					  kind: Option[String],
		  					  IBAN: Option[String],
		  					  bank: Option[LightBankJson],
		  					  metadata: Option[OtherAccountMetadataJson])
  
  case class TransactionValueJson(currency: Option[String],
		  						  amount: Option[String])
		  					  
  case class TransactionDetailsJson(`type`: Option[String],
		  							label: Option[String],
		  							posted: Option[Date], //TODO: Check if the default date formatter is okay
		  							completed: Option[Date], //TODO: Check if the default date formatter is okay
		  							new_balance: Option[AccountBalanceJson],
		  							value: Option[TransactionValueJson])	  					  
		  					  
  case class TransactionCommentJson(id: Option[String],
		  				 date: Option[Date], //TODO: Check if the default date formatter is okay
		  				 value: Option[String],
		  				 user: Option[UserJson],
		  				 reply_to: Option[String])
  
  case class TransactionTagJson(id: Option[String],
		  			 date: Option[Date], //TODO: Check if the default date formatter is okay
		  			 value: Option[String],
		  			 user: Option[UserJson])
  
  case class TransactionImageJson(id: Option[String],
		  						  label: Option[String],
		  						  URL: Option[String])
  
  case class TransactionMetadataJson(narrative: Option[String],
		  							 comments: Option[List[TransactionCommentJson]],
		  							 tags: Option[List[TransactionTagJson]],
		  							 images: Option[List[TransactionImageJson]],
		  							 where: Option[LocationJson])
  
  case class TransactionJson(uuid: Option[String],
		  					 id: Option[String],
		  					 this_account: Option[ThisAccountJson],
		  					 other_account: Option[OtherAccountJson],
		  					 details: Option[TransactionDetailsJson],
		  					 metadata: Option[TransactionMetadataJson])
}