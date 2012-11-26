package code.snippet

import code.model.dataAccess.{OBPUser,HostedBank}
import net.liftweb.common.{Full,Empty}
import scala.xml.NodeSeq
import net.liftweb.util.CssSel
import net.liftweb.util.Helpers._
import net.liftweb.http.SHtml 

class AccountRegistration{
	
	def render = {
		OBPUser.currentUser match {
			case Full(user) => {
				var banks : List[(String, String)] = HostedBank.findAll.map(t => (t.id.get.toString,t.name.get))
				banks ::= ("0","--> Choose a Bank")
				def bankId(id :String) = id
				"#bankList" #> SHtml.select(banks,Empty,bankId _) &
				"#loginMsg * " #> ""	
			}
			case _ => 
			"#submitAccount" #> NodeSeq.Empty &
			"#loginMsg * " #> "You need to login to submit you account"  
		}
	}
}