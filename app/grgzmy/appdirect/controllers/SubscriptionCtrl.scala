package grgzmy.appdirect.controllers

import grgzmy.appdirect.views.html.index
import oauth.signpost.basic.DefaultOAuthConsumer
import play.api.libs.oauth.ConsumerKey
import play.api.mvc._
import play.api.libs.ws._
import play.api.libs.oauth._
import play.api.Logger
import play.api.Play.current
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import grgzmy.appdirect.Security
import oauth.signpost._

class SubscriptionCtrl extends Controller {

  private def verify(implicit request: Request[AnyContent]): Either[Future[play.api.mvc.Result], Unit] = {
    val accessTokenOpt: Option[String] = request.headers.get("Authorization")
    (accessTokenOpt, Security.consumer) match {
      case (Some(accessToken), Some(consumer)) => {
        val calculator = new OAuthCalculator(consumer, RequestToken("", ""))
        val expected = WS.url(s"http://${request.host}${request.uri}").sign(calculator)
        expected.headers.get(Security.AUTH_HEADER)match {
          case Some(exp) if exp.contains(accessToken) =>
            Logger.info("verified request - received auth token is expected")
            Right(Unit)
          case Some(exp) =>
            Logger.info(s"oauth token mismatch. expected $exp, but got $accessToken")
            Left(Future.successful(Unauthorized("Cannot verify auth tokens")))
          case e =>
            Logger.info(s"some auth tokens missing expected $e but got $accessToken")
            Left(Future.successful(Unauthorized("Cannot verify auth tokens")))
        }
      }
      case (None, Some(c)) => Left(Future.successful(BadRequest("Missing OAuth headers!")))
      case _ => Left(Future.successful(InternalServerError("OAuth Error")))
    }
  }

  private def verifyWithJava(implicit request: Request[AnyContent]): Either[Future[play.api.mvc.Result], Unit] = {
    val accessTokenOpt: Option[String] = request.headers.get(Security.AUTH_HEADER)
    (accessTokenOpt, Security.javaConsumer) match {
      case (Some(accessToken), Some(consumer)) => {
        val consumer = Security.javaConsumer.get
        val expected = consumer.sign(s"http://${request.host}${request.uri}")
        expected match {
          case `accessToken` =>
            Logger.info("verified request - received auth token is expected")
            Right(Unit)
          case other =>
            Logger.info(s"oauth token mismatch. expected $other, but got $accessToken")
            Left(Future.successful(Unauthorized("Cannot verify auth tokens")))
        }
      }
      case (None, Some(c)) => Left(Future.successful(BadRequest("Missing OAuth headers!")))
      case _ => Left(Future.successful(InternalServerError("OAuth Error")))
    }
  }

  private def processAppDirectEvent(fetchUrl: String)(implicit request: Request[AnyContent]) = {
    Logger.info(s"fetching event from url $fetchUrl")
    WS.url(fetchUrl).sign(OAuthCalculator(Security.consumer.get, RequestToken("", ""))).get().map{
      r => {
        Logger.info(s"recieved response for the event :${r.xml.toString}")
        r
      }
    }

  }

  def order(fetchUrl: String) = Action.async{
    implicit request =>
      Logger.info(s"recieved order request from AppDirect: ${request.toString}")

      verifyWithJava match{
        case Left(r: Future[Result]) => r
        case Right(_) => processAppDirectEvent(fetchUrl).map{
          r =>
            val resp =
              <result>
                <success>true</success>
                <message>Account creation successful</message>
                <accountIdentifier>new-account-identifier</accountIdentifier>
              </result>
            Logger.info(s"replying back with dummy sucess ${resp.toString}")
            Ok(resp)
        }
      }

  }



}


