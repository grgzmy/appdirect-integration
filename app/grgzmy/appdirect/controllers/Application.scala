package grgzmy.appdirect.controllers

import grgzmy.appdirect.models.EventDao
import grgzmy.appdirect.views.html.index
import play.api._
import play.api.mvc._
import play.api.Play.current

class Application extends Controller {
  val eventDao = EventDao

  def index2 = Action {
    Ok(index(eventDao.getSubscriptions))
  }

  def someShit: Option[Int] = Some(3).map(_*2)

}
