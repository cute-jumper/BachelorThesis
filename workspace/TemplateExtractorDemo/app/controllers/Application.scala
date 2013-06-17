package controllers

import play.api._
import play.api.mvc._
import play.api.libs.ws.WS
import play.api.libs.concurrent.Execution.Implicits._
import thu.ailab.utils.MyCharsetDetector

object Application extends Controller {
  TemplateExtractor
  def index = Action {
    Ok(views.html.index("Your new application is ready."))
  }
  def blog(url: String) = newsLocal(url)
  def blogWeb(url: String) = Action { implicit request =>
    Async {
      WS.url(url).get().map { response =>
        val re = """.*charset=([^ "/>]*)""".r
        val htmlString =
          re findFirstIn response.header("content-type").get match {
            case Some(re(charset)) => response.body //TODO 
            case _ => re findFirstIn response.body match {
              case Some(re(contentCharset)) =>
                new String(response.body.toCharArray.map(_.toByte), contentCharset)
              case _ => response.body
            }
          }
        println(response.header("content-type"))
        val renderHTML = TemplateExtractor.feed(htmlString)
        if (renderHTML.isDefined)
          Ok(renderHTML.get).as(HTML)
        else
          Ok("No Template!").as(HTML)
      }
    }
  }
  def news(url: String) = newsLocal(url)
  def newsLocal(url: String) = Action { implicit request =>
    val charset = MyCharsetDetector.detectFile(url).get
    val htmlString = scala.io.Source.fromFile(url)(charset).getLines.mkString
    val renderHTML = TemplateExtractor.feed(htmlString)
    if (renderHTML.isDefined)
      Ok(renderHTML.get).as(HTML)
    else
      Ok("No Template!").as(HTML)
  }
}