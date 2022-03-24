package code

import net.liftweb.util.Props

object Constant {
  final val CUSTOM_OWNER_VIEW_ID = "owner"
  final val versionOfApi = Props.get("api_version").getOrElse("v4.0.0")
  final val versionOfApi121 = "v1.2.1"
  final val correlatedUserIdTargetCookieName = "CORRELATED_USER_ID_TARGET"
  final val correlatedUserIdBoundCookieName = "CORRELATED_USER_ID_BOUND"
  final val correlatedCustomerIdCreatedCookieName = "CORRELATED_CUSTOMER_ID_CREATED"
  final val linkBetweenCorrelatedUserAndCustomerCreatedCookieName = "LINK_BETWEEN_CORRELATED_USER_AND_CUSTOMER_CREATED"
}
