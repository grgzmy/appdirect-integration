package grgzmy.appdirect.controllers

import grgzmy.appdirect.views.html.index
import play.api.libs.oauth.ConsumerKey
import play.api.mvc._
import play.api.libs.ws._
import play.api.libs.oauth._
import play.api.Logger
import play.api.Play.current
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import grgzmy.appdirect.Security

class SubscriptionCtrl extends Controller {

  private def verify(implicit request: Request[AnyContent]): Either[Future[play.api.mvc.Result], Unit] = {
    val accessTokenOpt: Option[String] = request.headers.get("Authorization")
    (accessTokenOpt, Security.consumer) match {
      case (Some(accessToken), Some(consumer)) => {
        val expected = WS.url(s"http://${request.host}${request.uri}").sign(OAuthCalculator(consumer, RequestToken("", "")))
        (expected.headers.get(Security.AUTH_HEADER), (request.headers.get(Security.AUTH_HEADER))) match {
          case (Some(exp), Some(actual)) if exp == actual => Right(Unit)
          case _ => Left(Future.successful(Unauthorized("Cannot verify auth tokens")))
        }
      }
      case (None, Some(c)) => Left(Future.successful(BadRequest("Missing OAuth headers!")))
      case _ => Left(Future.successful(InternalServerError("OAuth Error")))
    }
  }

  private def processAppDirectEvent(fetchUrl: String)(implicit request: Request[AnyContent]) = {
    WS.url(fetchUrl).sign(OAuthCalculator(Security.consumer.get, RequestToken("", ""))).get().map{
      r => Logger.info(r.toString)
    }

  }

  def order(fetchUrl: String) = Action.async{
    implicit request =>
      verify match{
        case Left(r: Future[Result]) => r
        case Right(_) =>
          Future(processAppDirectEvent(fetchUrl))
          Future.successful(Ok)
      }

  }



}


