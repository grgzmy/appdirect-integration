package grgzmy.appdirect.controllers

import play.api.mvc._
import play.api.libs.ws._
import play.api.libs.oauth._
import play.api.Logger
import play.api.Play.current
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import grgzmy.appdirect.{models, Security}
import grgzmy.appdirect.models._
import grgzmy.appdirect.models.{SuccessResponse, FailureResponse}
import scala.xml.NodeSeq

class SubscriptionCtrl extends Controller {

  val eventDao: EventDao = new EventDao

//  private def verify(implicit request: Request[AnyContent]): Either[Future[play.api.mvc.Result], Unit] = {
//
//    val accessTokenOpt: Option[String] = request.headers.get("Authorization")
//    (accessTokenOpt, Security.consumer) match {
//      case (Some(accessToken), Some(consumer)) => {
//        val calculator = new OAuthCalculator(consumer, RequestToken("", ""))
//        val expected = WS.url(s"http://${request.host}${request.uri}").sign(calculator)
//        expected.headers.get(Security.AUTH_HEADER)match {
//          case Some(exp) if exp.contains(accessToken) =>
//            Logger.info("verified request - received auth token is expected")
//            Right(Unit)
//          case Some(exp) =>
//            Logger.info(s"oauth token mismatch. expected $exp, but got $accessToken")
//            Left(Future.successful(Unauthorized("Cannot verify auth tokens")))
//          case e =>
//            Logger.info(s"some auth tokens missing expected $e but got $accessToken")
//            Left(Future.successful(Unauthorized("Cannot verify auth tokens")))
//        }
//      }
//      case (None, Some(c)) => Left(Future.successful(BadRequest("Missing OAuth headers!")))
//      case _ => Left(Future.successful(InternalServerError("OAuth Error")))
//    }
//  }
//
//  private def verifyWithJava(implicit request: Request[AnyContent]): Either[Future[play.api.mvc.Result], Unit] = {
//    val accessTokenOpt: Option[String] = request.headers.get(Security.AUTH_HEADER)
//    (accessTokenOpt, Security.javaConsumer) match {
//      case (Some(accessToken), Some(consumer)) => {
//        val consumer = Security.javaConsumer.get
//          consumer.setSendEmptyTokens(true)
//        val expected = consumer.sign(s"http://${request.host}${request.uri}")
//        expected match {
//          case `accessToken` =>
//            Logger.info("verified request - received auth token is expected")
//            Right(Unit)
//          case other =>
//            Logger.info(s"oauth token mismatch. expected $other, but got $accessToken")
//            Left(Future.successful(Unauthorized("Cannot verify auth tokens")))
//        }
//      }
//      case (None, Some(c)) => Left(Future.successful(BadRequest("Missing OAuth headers!")))
//      case _ => Left(Future.successful(InternalServerError("OAuth Error")))
//    }
//  }

  private def fetchEvent(fetchUrl: String)(implicit request: Request[AnyContent]) = {
    Logger.info(s"fetching event from url $fetchUrl")
    WS.url(fetchUrl).sign(OAuthCalculator(Security.consumer.get, RequestToken("", ""))).get().map{
      r => {
        Logger.info(s"recieved response for the event :${r.xml.toString()}")
        r
      }
    }
  }


  private def order(event: Event, xml: NodeSeq): Future[Response] ={
    Future(eventDao.createSubscription(event, xml)).map{
      case Some(id)=> SuccessResponse("Created Account successfully", Some(id.toString))
      case None => FailureResponse("Could not create Account", ErrorCode.UNKNOWN_ERROR)
    }.recover{
      case e: DbException => e.printStackTrace()
        FailureResponse(e.getMessage, e.err)
      case e: Exception =>
        e.printStackTrace()
        FailureResponse(e.getMessage, ErrorCode.UNKNOWN_ERROR)
    }
  }

  private def change(event: Event, xml: NodeSeq): Future[Response] ={
    Future(eventDao.changeSubscription(event, xml)).map{
      case true=> SuccessResponse("Changed Account successfully", None)
      case false => FailureResponse("Account not found", ErrorCode.ACCOUNT_NOT_FOUND)
    }.recover{
      case e: EventParamException => FailureResponse("Malformed XML response", ErrorCode.INVALID_RESPONSE)
      case e: DbException => e.printStackTrace()
        FailureResponse(e.getMessage, e.err)
      case e: Exception =>
        e.printStackTrace()
        FailureResponse(e.getMessage, ErrorCode.UNKNOWN_ERROR)
    }
  }

  private def cancel(event: Event, xml: NodeSeq): Future[Response] ={
    Future(eventDao.cancelSubscription(event, xml)).map{
      case true=> SuccessResponse("Cancelled Account successfully", None)
      case false => FailureResponse("Account not found", ErrorCode.ACCOUNT_NOT_FOUND)
    }.recover{
      case e: EventParamException => FailureResponse("Malformed XML response", ErrorCode.INVALID_RESPONSE)
      case e: DbException => e.printStackTrace()
        FailureResponse(e.getMessage, e.err)
      case e: Exception =>
        e.printStackTrace()
        FailureResponse(e.getMessage, ErrorCode.UNKNOWN_ERROR)
    }
  }

  private def notice(event: Event, xml: NodeSeq): Future[Response] ={
    Future(eventDao.cancelSubscription(event, xml)).map{
      case true=> SuccessResponse("Modified Account Status successfully", None)
      case false => FailureResponse("Account not found", ErrorCode.ACCOUNT_NOT_FOUND)
    }.recover{
      case e: EventParamException => FailureResponse("Malformed XML response", ErrorCode.INVALID_RESPONSE)
      case e: DbException => e.printStackTrace()
        FailureResponse(e.getMessage, e.err)
      case e: Exception =>
        e.printStackTrace()
        FailureResponse(e.getMessage, ErrorCode.UNKNOWN_ERROR)
    }
  }

  private def unkownEvent(event: Event, xml: NodeSeq): Future[Response] = Future.successful(FailureResponse("Unknown Event", ErrorCode.INVALID_RESPONSE))

  val actions: Map[EventType.Value, (Event, NodeSeq)=>Future[Response]] = Map(
    EventType.SUBSCRIPTION_ORDER -> order,
    EventType.SUBSCRIPTION_CHANGE -> change,
    EventType.SUBSCRIPTION_NOTICE -> notice,
    EventType.SUBSCRIPTION_CANCEL -> cancel,
    EventType.UNKNOWN ->  unkownEvent
  )

  def event(fetchUrl: String) = Action.async{
    implicit request =>
      Logger.info(s"recieved order request from AppDirect: ${request.toString()}")
      fetchEvent(fetchUrl).flatMap(resp =>
        {
          val e = Event.from(resp.xml)
          actions(e.typ)(e, resp.xml).map(r => {
            Logger.info(s"sending back: ${r.toXml.toString}")
            Ok(r.toXml)})
        }
      )

  }

//  def parse(raw: String): Map[String, String] = {
//    raw.filterNot(List(' ,',  '"').contains).split(" ").toSeq.tail.map(_.split("=")).map(_.toList).map(r => (r.head, if(r.length > 1) r.last else "")).toMap
//  }
//
//  def test = Action{
//    //WS.url("http://localhost:1337/test2").sign(OAuthCalculator(Security.consumer.get, RequestToken("", ""))).get().map(r => Ok(r.body))
//    try{
//      val stmt = conn.createStatement()
//      val rs = stmt.executeQuery("SELECT * FROM BRO")
//      rs.next()
//      Ok(s"""got back ${rs.getString("NAME")}""")
//    } finally{
//      conn.close()
//    }
//
//  }
//
//  def test2 = Action{
//    implicit request =>{
//      val got = request.headers.get(signpost.OAuth.HTTP_AUTHORIZATION_HEADER)
//      val nonce = got.map(parse(_)).map(_("oauth_nonce")).get
//      val exp = Security.javaConsumer.map(c => {
//        c.setSendEmptyTokens(true)
//        val p = new HttpParameters()
//        p.put("oauth_nonce", nonce, true)
//        c.setAdditionalParameters(p)
//        c
//      }).get.sign("http://localhost:1337/test2")
//
//    val r = Map("got" -> request.headers.get(signpost.OAuth.HTTP_AUTHORIZATION_HEADER), "expected" -> exp)
//    Ok(r.toString())
//  }}


}






