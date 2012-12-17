package code.model.traits

import net.liftweb.json.JsonDSL._
import net.liftweb.json.JsonAST.JObject

trait User {
  def id_ : String
  def emailAddress : String
  def theFistName : String
  def theLastName : String
  def permittedViews(bankAccount: BankAccount) : Set[View]
  def hasMangementAccess(bankAccount: BankAccount)  : Boolean
  def accountsWithMoreThanAnonAccess : Set[BankAccount]
  def toJson : JObject = 
    ("id" -> id_) ~
    ("provider" -> "sofi.openbankproject.com") ~
    ("display_name" -> {theFistName + " " + theLastName})
}