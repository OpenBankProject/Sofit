package code.api

import net.liftweb.http.rest.RestHelper
import net.liftweb.http.Req
import net.liftweb.common._
import net.liftweb.http.LiftResponse
import net.liftweb.http.JsonResponse
import code.util.APIUtil._
import code.model.traits.User
import code.api.OAuthHandshake._

class OBPRestHelper extends RestHelper {

  implicit def jsonResponseBoxToJsonReponse(box: Box[JsonResponse]): JsonResponse = {
    box match {
      case Full(r) => r
      case Failure(msg, _, _) => errorJsonResponse(msg)
      case _ => errorJsonResponse()
    }
  }

  def failIfBadOauth(fn: (Box[User]) => JsonResponse) : JsonResponse = {
    if (isThereAnOAuthHeader) {
      getUser match {
        case Full(u) => fn(Full(u))
        case Failure(msg, _, _) => errorJsonResponse(msg)
        case _ => errorJsonResponse("oauth error")
      }
    } else fn(Empty)
  }
  
  override protected def serve(handler: PartialFunction[Req, () => Box[LiftResponse]]) : Unit= {
    
    val obpHandler : PartialFunction[Req, () => Box[LiftResponse]] = {
      new PartialFunction[Req, () => Box[LiftResponse]] {
        def apply(r : Req) = {
          //Wraps the partial function with some logging
          logAPICall
          handler(r)
        }
        def isDefinedAt(r : Req) = handler.isDefinedAt(r)
      }    
    }
    super.serve(obpHandler)
  }
  
  
}