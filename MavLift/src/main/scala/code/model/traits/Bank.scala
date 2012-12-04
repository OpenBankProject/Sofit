package code.model.traits
import net.liftweb.common.Box
import code.model.dataAccess.LocalStorage
import net.liftweb.json.JsonAST.JObject
import net.liftweb.json.JsonDSL._
import net.liftweb.json.JsonAST.JArray

trait Bank 
{
	def id : String
	def name : String
	def permalink : String
	def accounts : Set[BankAccount]
	
	def toJson : JObject = {
	  ("alias" -> permalink) ~
      ("name" -> name) ~
      ("logo" -> "") ~
      ("links" -> linkJson)
	}
	
	def linkJson : JObject = {
      ("rel" -> "bank") ~
      ("href" -> {"/" + permalink + "/bank"}) ~
      ("method" -> "GET") ~
      ("title" -> {"Get information about the bank identified by " + permalink})
    }
}

object Bank {
  def apply(bankPermalink: String) : Box[Bank] = LocalStorage.getBank(bankPermalink)
  
  def all : List[Bank] = LocalStorage.allBanks
  
  def toJson(banks: Seq[Bank]) : JArray = 
    banks.map(bank => bank.toJson)
  
}