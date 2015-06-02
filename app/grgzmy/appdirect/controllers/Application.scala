package grgzmy.appdirect.controllers

import grgzmy.appdirect.views.html.index
import play.api._
import play.api.mvc._
import play.api.Play.current

class Application extends Controller {

  def index2 = Action {
    Ok(index(s"Your new application is ready. $someShit"))
  }

  def someShit: Option[Int] = Some(3).map(_*2)

}
