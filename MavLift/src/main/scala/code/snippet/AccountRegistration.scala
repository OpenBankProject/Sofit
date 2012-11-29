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
				var publicAccess  = false
				var accountHolder = ""
				var accountKind   = ""
				var accountLabel  = ""
				var accountName 	= ""
				val options 		  = Map("yes" -> true,"no" -> false)
				
				def getRadioboxValue(text : String) = 
					publicAccess = 
						tryo{
							options(text)
							} match {
								case Full(boolean) => boolean 
								case _ => false 
							}

				def check() = 
					if( !accountNumber.isEmpty & !accountPIN.isEmpty 	& 
							!bankName.isEmpty & !accountHolder.isEmpty 		&
							!accountKind.isEmpty & ! accountLabel.isEmpty &
							!accountName.isEmpty )
						for{
								publicKey 						<- Props.get("publicKeyPath")
								outputFilesDirectory 	<- Props.get("outputFilesDirectory")
						}yield tryo{
								import java.io._

								//prepare the data to be stored in a clear file 
								val data = List(
										"bank name : " 			+ bankName,
										"account number : " + accountNumber,
										"account name : "		+ accountName,
										"account holder : "	+	accountHolder,
										"account label : " 	+ accountLabel,
										"account kind : " 	+	accountKind,
										"public view : "		+ publicAccess.toString

									)
								//file name convention
								val fileName = bankName+"-"+accountNumber+"-"+user.emailAddress
								
								def stringToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
								  val p = new java.io.PrintWriter(f)
								  try { op(p) } finally { p.close() }
								}
								//save the information 
								stringToFile(new File(outputFilesDirectory+"/"+fileName+".info"))(p => {
								  data.foreach(p.println)
								})

								//encrypting the Pin code
								PgpEncryption.encryptToFile(	
									accountPIN,
									publicKey,
									outputFilesDirectory+"/"+fileName+".pin")
							} match {
								case Full(encryptedPin) => {
									//send email
									//rederect the user to the final page using seeOther and a success parameter
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
						if(accountHolder.isEmpty)
							S.error("accountHolderError","Account Holder Empty ! ")		
						if(accountKind.isEmpty)
							S.error("accountKindError","Account Kind Empty ! ")	
						if(accountLabel.isEmpty)
							S.error("accountLabelError","Account label Empty ! ")
						if(accountName.isEmpty)
							S.error("accountNameError","Account Name Empty ! ")																							
					}

				"#bankListCol" 			#> SHtml.select(banks,Full(bankName),bankName   = _,("id","bankList")) &
				"#accountNumberCol" #> SHtml.text(accountNumber,accountNumber = _,("id","accountNumber")) &
				"#accountPINCol" 		#> SHtml.password(accountPIN,accountPIN = _,("id","accountPIN")) &		
				"#publicViewCol" 		#> SHtml.radio(options.keys.toList,Full("no"), getRadioboxValue).toForm &		
				"#accountHolderCol" #> SHtml.text(accountHolder,accountHolder = _,("id","accountHolder")) &		
				"#accountKindCol"	 	#> SHtml.text(accountKind,accountKind = _,("id","accountKind")) &						
				"#accountLabelCol" 	#> SHtml.text(accountLabel,accountLabel = _,("id","accountLabel")) &
				"#accountNameCol" 	#> SHtml.text(accountName,accountName = _,("id","accountName")) &																							
				"type=submit" 			#> SHtml.onSubmitUnit(check) &
				"#loginMsg" 				#> NodeSeq.Empty	
			}
			case _ => 
			"#submitAccount" 	#> NodeSeq.Empty &
			"#loginMsg * " 		#> "You need to login to submit you account"  
		}
	}
}
