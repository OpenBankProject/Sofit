package code

import net.liftweb.util.Props

object Constant {
  final val CUSTOM_OWNER_VIEW_ID = "owner"
  final val versionOfApi = Props.get("api_version").getOrElse("v4.0.0")
  final val versionOfApi121 = "v1.2.1"
}
