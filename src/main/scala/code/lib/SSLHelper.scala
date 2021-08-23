package code.lib

import java.io.FileInputStream
import java.net.{HttpURLConnection, URL}
import java.security.{KeyStore, SecureRandom}

import javax.net.ssl.{TrustManagerFactory, _}
import net.liftweb.util.Props

object SSLHelper {


  private lazy val sSLSocketFactory = {
    val keystoreFile: String = Props.get("ssl_keystore_location").openOrThrowException("props value of ssl_keystore_location is missing")
    val keystorePassword = Props.get("ssl_keystore_password", "")
    val truststoreFile = Props.get("ssl_truststore_location","")
    val truststorePassword = Props.get("ssl_truststore_password", "")
    val keyPassword = Props.get("ssl_key_password", "")

    

    val tmf = TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm)
    val keyManager = KeyManagerFactory.getInstance(KeyManagerFactory.getDefaultAlgorithm)
    val keystore = KeyStore.getInstance(KeyStore.getDefaultType)
    val inputStream = new FileInputStream(keystoreFile)
    try {
      keystore.load(inputStream, keystorePassword.toCharArray)
    } finally {
      inputStream.close()
    }
    keyManager.init(keystore,keyPassword.toCharArray)
    
    val truststore = KeyStore.getInstance(KeyStore.getDefaultType)
    val trustInputStream = new FileInputStream(truststoreFile)
    try {
      truststore.load(trustInputStream, truststorePassword.toCharArray)
    } finally {
      trustInputStream.close()
    }


    tmf.init(truststore)
 
    

    val sslContext = SSLContext.getInstance("TLSv1.2")
    sslContext.init(keyManager.getKeyManagers, tmf.getTrustManagers, new SecureRandom())

    val hostnameVerifier: HostnameVerifier = new HostnameVerifier {
      override def verify(host: String, sslSession: SSLSession): Boolean = true
    }

    HttpsURLConnection.setDefaultHostnameVerifier(hostnameVerifier)

    sslContext.getSocketFactory
  }

  def getConnection(url: String): HttpURLConnection = {
    Props.get("ssl_client_auth", "false") match {
      case "true" => {
        val httpsUrl = if (url.startsWith("https://")) url else url.replaceFirst("^http://", "https://")

        val connection = new URL(httpsUrl).openConnection().asInstanceOf[HttpsURLConnection]
        connection.setSSLSocketFactory(sSLSocketFactory)
        connection
      }
      case "false" => new URL(url).openConnection().asInstanceOf[HttpURLConnection]
      case errorValue => sys.error(s"obp_certificate_activate props should be true or false, current value is: $errorValue")
    }
  }

}
