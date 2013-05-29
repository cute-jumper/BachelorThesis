package controllers

import play.api._
import play.api.mvc._
import play.api.libs.ws.WS
import play.api.libs.concurrent.Execution.Implicits._

object Application extends Controller {
  
  def index = Action {
    Ok(views.html.index("Your new application is ready."))
  }
  
  def blog(url: String) = Action { implicit request =>  
    Async {
      WS.url(url).get().map { response =>
        val re = """.*charset=([^ "/>]*)""".r
        val r = re findFirstIn response.header("content-type").get match {
          case Some(re(charset)) => response.body //TODO 
          case _ => re findFirstIn response.body match {
            case Some(re(contentCharset)) => 
              new String(response.body.toCharArray.map(_.toByte), contentCharset)
            case _ => response.body
          }
        }
        println(response.header("content-type"))
        Ok(r).as(HTML)
      }
    }
  }
}