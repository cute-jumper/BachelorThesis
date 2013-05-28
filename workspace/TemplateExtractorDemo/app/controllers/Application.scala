package controllers

import play.api._
import play.api.mvc._
import play.api.libs.ws.WS

object Application extends Controller {
  
  def index = Action {
    Ok(views.html.index("Your new application is ready."))
  }
  
  def blog(url: String) = Action { implicit request =>
    Async {
      WS.url(url).get().map{ response =>
        Ok("Blog content: ")
      }
    }
  }
}