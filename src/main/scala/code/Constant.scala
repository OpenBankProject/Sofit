package code

import net.liftweb.util.Props

object Constant {
  final val CUSTOM_OWNER_VIEW_ID = "owner"
  final val versionOfApi = Props.get("api_version").getOrElse("v4.0.0")
  final val versionOfApi121 = "v1.2.1"
  final val correlatedUserIdCookieName = "CORRELATED_USER_ID"
  final val correlatedCustomerIdCookieName = "CORRELATED_CUSTOMER_ID_BOUND"
  final val loggedOnUserIdCookieName = "LOGGED_ON__CUSTOMER_ID_BOUND"
  final val correlatedUserIdBoundCookieName = correlatedUserIdCookieName + "_BOUND"
}
