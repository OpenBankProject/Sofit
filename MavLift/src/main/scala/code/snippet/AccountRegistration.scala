package code.snippet

import code.model.dataAccess.{OBPUser,HostedBank}
import net.liftweb.common.{Full,Box,Empty}
import scala.xml.NodeSeq
import net.liftweb.util.CssSel
import net.liftweb.util.Helpers._
import net.liftweb.util.Props
import net.liftweb.http.{S,SHtml,RequestVar}
import code.pgp.PgpEncryption

class AccountRegistration {
  
	private object  bankName 			extends RequestVar("")
	private object  accountNumber extends RequestVar("")
	private object  accountPIN    extends RequestVar("")
	private object  publicAccess  extends RequestVar(false)
	private object  accountHolder extends RequestVar("")
	private object  accountKind   extends RequestVar("")
	private object  accountLabel  extends RequestVar("")
	private object  accountName 	extends RequestVar("") 

	def renderForm = {
		OBPUser.currentUser match {
			case Full(user) => {
				val banks 				= "Choose a Bank" :: HostedBank.findAll.map(_.name.get) 
				val options 			= Map("yes" -> true,"no" -> false)
				val optionsSwaped	= options.map{_.swap}
				def getBooleanValue(text : Box[String]) = 
					text match {
						case Full(value) => tryo{
							options(value)
							} match {
								case Full(boolean) => boolean 
								case _ => false 
							}
						case _ => false
					}
					
				def check() = 
					if( !accountNumber.is.isEmpty & !accountPIN.is.isEmpty 	& 
							!bankName.is.isEmpty & !accountHolder.is.isEmpty 		&
							!accountKind.is.isEmpty & ! accountLabel.is.isEmpty &
							!accountName.is.isEmpty )
						for{
								publicKey 						<- Props.get("publicKeyPath")
								outputFilesDirectory 	<- Props.get("outputFilesDirectory")
						}yield tryo{
								import java.io._

								//prepare the data to be stored in a clear file 
								val data = List(
										"bank name : " 			+ bankName.is,
										"account number : " + accountNumber.is,
										"account name : "		+ accountName.is,
										"account holder : "	+	accountHolder.is,
										"account label : " 	+ accountLabel.is,
										"account kind : " 	+	accountKind.is,
										"public view : "		+ publicAccess.is.toString

									)
								//file name convention
								val fileName = bankName.is+"-"+accountNumber.is+"-"+user.emailAddress
								
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
									accountPIN.is,
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
						if(bankName.is.isEmpty)
							S.error("bankError","Bank not selected ! ")							
						if(accountNumber.is.isEmpty)
							S.error("accountNumberError","Account Number Empty ! ")
						if(accountPIN.is.isEmpty)
							S.error("accountPINError","Account PIN Empty ! ")				
						if(accountHolder.is.isEmpty)
							S.error("accountHolderError","Account Holder Empty ! ")		
						if(accountKind.is.isEmpty)
							S.error("accountKindError","Account Kind Empty ! ")	
						if(accountLabel.is.isEmpty)
							S.error("accountLabelError","Account label Empty ! ")
						if(accountName.is.isEmpty)
							S.error("accountNameError","Account Name Empty ! ")																							
					}
				
				"#bankListCol" 			#> SHtml.selectElem(banks,Full(bankName.is),("id","bankList"))((v : String) => bankName.set(v)) &
				"#accountNumberCol" #> SHtml.textElem(accountNumber,("id","accountNumber")) &
				"#accountPINCol" 		#> SHtml.passwordElem(accountPIN,("id","accountPIN")) &		
				"#publicViewCol" 		#> SHtml.radioElem(options.keys.toList,Full(optionsSwaped(publicAccess.is)))((v : Box[String]) => publicAccess.set(getBooleanValue(v))).toForm &		
				"#accountHolderCol" #> SHtml.textElem(accountHolder,("id","accountHolder")) &		
				"#accountKindCol"	 	#> SHtml.textElem(accountKind,("id","accountKind")) &						
				"#accountLabelCol" 	#> SHtml.textElem(accountLabel,("id","accountLabel")) &
				"#accountNameCol" 	#> SHtml.textElem(accountName,("id","accountName")) &																							
				"type=submit" 			#> SHtml.onSubmitUnit(check) &
				"#loginMsg" 				#> NodeSeq.Empty	
			}
			case _ => 
			"#submitAccount" 	#> NodeSeq.Empty &
			"#loginMsg * " 		#> "You need to login to submit you account"  
		}
	}
}
