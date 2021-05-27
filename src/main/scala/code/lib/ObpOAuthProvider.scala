package code.lib

import java.io.IOException
import java.net.{HttpURLConnection, MalformedURLException}

import oauth.signpost.AbstractOAuthProvider
import oauth.signpost.basic.{HttpURLConnectionRequestAdapter, HttpURLConnectionResponseAdapter}
import oauth.signpost.http.{HttpRequest, HttpResponse}


/**
 * Library https://github.com/mttkay/signpost we use for OAuth1 does not implement MTLS connection.
 * In order to support it ObpOAuthProvider inherit AbstractOAuthProvider and override function createRequest
 * in a way that {@link HttpURLConnection} is used in case the props ssl_client_auth=false
 * or {@link HttpsURLConnection} in case the props ssl_client_auth=true
 * to receive tokens from a service provider.
 */
@SerialVersionUID(1L)
class ObpOAuthProvider(val requestTokenEndpointUrl: String, val accessTokenEndpointUrl: String, val authorizationWebsiteUrl: String) extends AbstractOAuthProvider(requestTokenEndpointUrl, accessTokenEndpointUrl, authorizationWebsiteUrl) {
  @throws[MalformedURLException]
  @throws[IOException]
  override protected def createRequest(endpointUrl: String): HttpRequest = {
    val connection = SSLHelper.getConnection(endpointUrl)
    connection.setRequestMethod("POST")
    connection.setAllowUserInteraction(false)
    connection.setRequestProperty("Content-Length", "0")
    new HttpURLConnectionRequestAdapter(connection)
  }

  @throws[IOException]
  override protected def sendRequest(request: HttpRequest): HttpResponse = {
    val connection = request.unwrap.asInstanceOf[HttpURLConnection]
    connection.connect()
    new HttpURLConnectionResponseAdapter(connection)
  }

  override protected def closeConnection(request: HttpRequest, response: HttpResponse): Unit = {
    val connection = request.unwrap.asInstanceOf[HttpURLConnection]
    if (connection != null) connection.disconnect()
  }
}
