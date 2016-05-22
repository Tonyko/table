package controllers

import play.api._
import play.api.libs.json.{Json, OFormat}
import play.api.mvc._
import scalikejdbc._
import scalikejdbc.config.DBs

case class DataTableResult(data: List[List[String]], recordsTotal: Int, recordsFiltered: Int)

class Application extends Controller {

  DBs.setupAll()
  implicit val session = AutoSession
  implicit val dataTableResultFormat: OFormat[DataTableResult] =
    Json.format[DataTableResult]

  case class Record(id: Int, who: String, whom: String, ammount: Double, signature: Long)

  object Record extends SQLSyntaxSupport[Record] {
    def apply(rs: WrappedResultSet) = new Record(
      rs.int("id"), rs.string("who"), rs.string("whom"), rs.double("ammount"), rs.long("signature"))
  }

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

  def records = Action {
    val records: List[Record] = sql"select * from records".map(rs => Record(rs)).list.apply
    Ok(Json.toJson(records.size))
  }

  def add = Action(parse.json) { implicit request =>
    (for {
      who <- (request.body \ "from").asOpt[String]
      whom <- (request.body \ "to").asOpt[String]
      amount <- (request.body \ "amount").asOpt[Double]
      signature <- (request.body \ "signature").asOpt[Long]
    } yield {
      sql"insert into records(who,whom,ammount,signature) values ($who, $whom, $amount, $signature)".update.apply()
      Ok
    }) getOrElse {
      Logger.error(request.body.toString())
      Logger.error((request.body \ "from").asOpt[String].getOrElse("error"))
      Logger.error((request.body \ "to").asOpt[String].getOrElse("error"))

      BadRequest
    }
  }

}