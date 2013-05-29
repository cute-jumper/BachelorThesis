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
      val req = WS.url(url)
      req.withHeaders(("user-agent",
          "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:21.0) Gecko/20100101 Firefox/21.0"))          
      req.get().map { response =>
        val r = if (response.body.contains("gb2312"))
          new String(response.body.toCharArray.map(_.toByte), "utf-8")
        else
          response.body
        val f = new java.io.PrintWriter("/home/qjp/tmp/r.html")
        f.write(r)
        f.close
        Ok(r).as("text/html")
      }
    }
  }
}