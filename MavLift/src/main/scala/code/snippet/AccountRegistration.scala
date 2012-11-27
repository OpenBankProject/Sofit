package code.snippet

import code.model.dataAccess.{OBPUser,HostedBank}
import net.liftweb.common.{Full,Empty}
import scala.xml.NodeSeq
import net.liftweb.util.CssSel
import net.liftweb.util.Helpers._
import net.liftweb.http.SHtml 
import net.liftweb.http.S
import net.liftweb.util.Props
import code.pgp.PgpEncryption

class AccountRegistration {
	def renderForm = {
		OBPUser.currentUser match {
			case Full(user) => {
				var banks : List[(String, String)] = HostedBank.findAll.map(t => (t.name.get,t.name.get)) 
				banks ::= ("","Choose a Bank")
				
				var bankName      = ""
				var accountNumber = ""
				var accountPIN    = ""

				def check() = 
					if( !accountNumber.isEmpty & !accountPIN.isEmpty & !bankName.isEmpty)
						for{
								publicKey 						<- Props.get("publicKeyPath")
								outputFilesDirectory 	<- Props.get("outputFilesDirectory")
						}yield tryo{
								PgpEncryption.encryptToFile(	
									accountPIN,
									publicKey,
									outputFilesDirectory+"/"+bankName+"-"+accountNumber+"-"+user.emailAddress+".pin")
							} match {
								case Full(encryptedPin) => {
									//rederect the user to the final page
								}
								case _ => //redirect the user to the problem page
							}
					else
					{
						if(bankName.isEmpty)
							S.error("bankError","Bank not selected ! ")							
						if(accountNumber.isEmpty)
							S.error("accountNumberError","Account Number Empty ! ")
						if(accountPIN.isEmpty)
							S.error("accountPINError","Account PIN Empty ! ")							
					}
				"#bankListCol * "#> SHtml.select(banks,Full(bankName),bankName = _,("id","bankList")) &
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
