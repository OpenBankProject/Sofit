/**
Open Bank Project - Transparency / Social Finance Web Application
Copyright (C) 2011, 2012, TESOBE / Music Pictures Ltd

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

Email: contact@tesobe.com 
TESOBE / Music Pictures Ltd 
Osloerstrasse 16/17
Berlin 13359, Germany

  This product includes software developed at
  TESOBE (http://www.tesobe.com/)
  by 
  Simon Redfern : simon AT tesobe DOT com
  Stefan Bethge : stefan AT tesobe DOT com
  Everett Sochowski : everett AT tesobe DOT com
  Ayoub Benali: ayoub AT tesobe DOT com
 */

import net.liftweb.util.Props
import org.eclipse.jetty.server.{Connector, HttpConfiguration, HttpConnectionFactory, SecureRequestCustomizer, ServerConnector, SslConnectionFactory, _}
import org.eclipse.jetty.util.ssl.SslContextFactory
import org.eclipse.jetty.webapp.WebAppContext

object RunMtlsWebApp extends App {
  val server = new Server
  //set run mode value to "development", So the value is true of Props.devMode
  System.setProperty("run.mode", "development")
  val devPort: Int = Props.get("dev.port", "8081").toInt

  // set MTLS
  val connectors: Array[Connector] = {
    val https = new HttpConfiguration
    https.addCustomizer(new SecureRequestCustomizer)

    val sslContextFactory = new SslContextFactory()
    sslContextFactory.setKeyStorePath(this.getClass.getResource("/cert/server.jks").toExternalForm)
    sslContextFactory.setKeyStorePassword("123456")

    sslContextFactory.setTrustStorePath(this.getClass.getResource("/cert/server.trust.jks").toExternalForm)
    sslContextFactory.setTrustStorePassword("123456")
    sslContextFactory.setNeedClientAuth(true)

    sslContextFactory.setProtocol("TLSv1.2")

    val connector = new ServerConnector(server, new SslConnectionFactory(sslContextFactory, "http/1.1"), new HttpConnectionFactory(https))
    connector.setPort(devPort)

    Array(connector)
  }
  server.setConnectors(connectors)

  val context = new WebAppContext()
  context.setServer(server)
  context.setContextPath("/")
  context.setWar("src/main/webapp")

  server.setHandler(context)

  try {
    println(s">>> STARTING EMBEDDED JETTY SERVER, Start https at port $devPort, PRESS ANY KEY TO STOP")
    server.start()
    while (System.in.available() == 0) {
      Thread.sleep(5000)
    }
    server.stop()
    server.join()
  } catch {
    case exc : Exception => {
      exc.printStackTrace()
      sys.exit(100)
    }
  }
}
