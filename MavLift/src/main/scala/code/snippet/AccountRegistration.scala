package code.snippet

import code.model.dataAccess.{OBPUser,HostedBank}
import net.liftweb.common.{Full,Empty}
import scala.xml.NodeSeq
import net.liftweb.util.CssSel
import net.liftweb.util.Helpers._
import net.liftweb.http.SHtml 
import net.liftweb.http.S
import code.pgp.PgpEncryption

class AccountRegistration {
	def renderForm = {
		OBPUser.currentUser match {
			case Full(user) => {
				var banks : List[(String, String)] = HostedBank.findAll.map(t => (t.id.get.toString,t.name.get)) 
				banks ::= ("0","Choose a Bank")
				
				var bankId="0"
				var accountNumber = ""
				var accountPIN = ""

				def check() = 
					if( !accountNumber.isEmpty & !accountPIN.isEmpty & bankId!="0")
					{
						println("--> all the parameters are corrects")
//						PgpEncryption.encryptToFile()
						//encryte the file
					}
					else
					{
						if(bankId=="0")
							S.error("bankError","Bank not selected ! ")							
						if(accountNumber.isEmpty)
							S.error("accountNumberError","Account Number Empty ! ")
						if(accountPIN.isEmpty)
							S.error("accountPINError","Account PIN Empty ! ")							
					}
				"#bankListCol * "#> SHtml.select(banks,Full(bankId),bankId = _,("id","bankList")) &
				"#accountNumberCol *" #> SHtml.text(accountNumber,accountNumber = _,("id","accountNumber")) &
				"#accountPINCol *" #> SHtml.password(accountPIN,accountPIN = _,("id","accountPIN")) &		
				"type=submit" #> SHtml.onSubmitUnit(check) &
				"#loginMsg" #> NodeSeq.Empty	
			}
			case _ => 
			"#submitAccount" #> NodeSeq.Empty &
			"#loginMsg * " #> "You need to login to submit you account"  
		}
	}
}
