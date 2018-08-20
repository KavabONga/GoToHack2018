import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.stream.ActorMaterializer
import slick.basic.DatabaseConfig

import scala.io.StdIn
import slick.jdbc
import slick.jdbc.{JdbcProfile, SQLiteProfile}

import scala.concurrent.ExecutionContext.Implicits.global
import slick.jdbc.SQLiteProfile.api._

case class User(id: Option[Int], closeKey: Int, openKey: Int)
case class Key(vector: Array[Boolean])

//h2mem1 = {
//  url = "jdbc:h2:mem:test1"
//  driver = org.h2.Driver
//  connectionPool = disabled
//  keepAliveConnection = true
//}
//val db = Database.forConfig("h2mem1")
//try {
//  // ...
//} finally db.close

trait Db {
  val config: DatabaseConfig[SQLiteProfile]
  val db: JdbcProfile#Backend#Database = config.db
}
private class Users(tag: Tag) extends Table[User](tag, "USERS") {
  def id = column[Int]("ADDRESS_ID", O.PrimaryKey, O.AutoInc)
  def closeKey = column[Int]("CLOSE_KEY")
  def openKey = column[Int]("OPEN_KEY")
  def * = (id.?, closeKey, openKey) <> (User.tupled, User.unapply)
}
//val users = TableQuery[Users]

object DBMain extends App {
  val db = Database.forURL("jdbc:sqlite:/home/sq/workspace/dbFun/IOdb.db", driver = "org.sqlite.JDBC")
  val action = sql"select CATEGORY from MAINTENANCE_REQUEST".as[(Int)]
  db.run(action).foreach(println)
}
