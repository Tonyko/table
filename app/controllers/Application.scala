package controllers

import play.api._
import play.api.libs.json.{Json, OFormat}
import play.api.mvc._

case class DataTableResult(data: List[List[String]], recordsTotal: Int, recordsFiltered: Int)

class Application extends Controller {

  implicit val dataTableResultFormat: OFormat[DataTableResult] =
    Json.format[DataTableResult]

  def index = Action {
    Ok(views.html.index("Your new application is ready."))
  }

  def list = Action(parse.json) { implicit request =>
    (for {
      offset <- (request.body \ "offset").asOpt[Int]
      limit <- (request.body \ "limit").asOpt[Int]
    } yield {
      val testList: List[String] = (1 to 100000).map(x => x.toString).toList
      val a = testList.map(x => List(x))
      val result = a.slice(offset, offset + limit)

      val jsonObject = Json.toJson(
        DataTableResult(
          result,
          a.size,
          a.size
        )
      )

      Ok(jsonObject)
    }) getOrElse {
      BadRequest
    }
  }

}