package grgzmy.appdirect.controllers

import play.api.mvc._
import play.api.libs.ws._
import play.api.libs.oauth._
import play.api.Logger
import play.api.Play.current
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import grgzmy.appdirect.Security
import grgzmy.appdirect.models._
import grgzmy.appdirect.models.{SuccessResponse, FailureResponse}
import scala.xml.NodeSeq

class EventCtrl extends Controller {

  val eventDao = EventDao

  def event(fetchUrl: String) = Action.async{
    implicit request =>
      Logger.info(s"received order request from AppDirect: ${request.toString()}")
      fetchEvent(fetchUrl).flatMap(resp =>
      {
        val e = Event.from(resp.xml)
        actions(e.typ)(e, resp.xml).map(r => {
          Logger.info(s"sending back: ${r.toXml.toString()}")
          Ok(r.toXml)})
      }
      )

  }

  private def fetchEvent(fetchUrl: String)(implicit request: Request[AnyContent]) = {
    Logger.info(s"fetching event from url $fetchUrl")
    WS.url(fetchUrl).sign(OAuthCalculator(Security.consumer.get, RequestToken("", ""))).get().map{
      r => {
        Logger.info(s"received response for the event :${r.xml.toString()}")
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

  private def userAssignment(event: Event, xml: NodeSeq): Future[Response] =
    Future(eventDao.assignUser(event, xml)).map(r => SuccessResponse("Assigned users successfully", None)).recover{
      case e: EventParamException => FailureResponse("Malformed XML response", ErrorCode.INVALID_RESPONSE)
      case e: DbException => e.printStackTrace()
        FailureResponse(e.getMessage, e.err)
      case e: Exception =>
        e.printStackTrace()
        FailureResponse(e.getMessage, ErrorCode.UNKNOWN_ERROR)
    }


  private def userUnassignment(event: Event, xml: NodeSeq): Future[Response] ={
    Future(eventDao.unassignUser(event, xml)).map{
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

  private def unknownEvent(event: Event, xml: NodeSeq): Future[Response] = Future.successful(FailureResponse("Unknown Event", ErrorCode.INVALID_RESPONSE))

  private val actions: Map[EventType.Value, (Event, NodeSeq)=>Future[Response]] = Map(
    EventType.SUBSCRIPTION_ORDER -> order,
    EventType.SUBSCRIPTION_CHANGE -> change,
    EventType.SUBSCRIPTION_NOTICE -> notice,
    EventType.SUBSCRIPTION_CANCEL -> cancel,
    EventType.USER_ASSIGNMENT -> userAssignment,
    EventType.USER_UNASSIGNMENT -> userUnassignment,
    EventType.UNKNOWN ->  unknownEvent
  )

}






