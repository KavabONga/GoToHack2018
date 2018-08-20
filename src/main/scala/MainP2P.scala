import org.json4s.jackson.JsonMethods.{compact, parse, render}
import org.json4s.JsonDSL._
import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, Props, Status}
import akka.http.scaladsl.server.{Route, StandardRoute}
import akka.http.scaladsl.{Http, server}
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import akka.stream.ActorMaterializer

import scala.xml._
import scala.xml.Utility.trim
import scala.io.StdIn
import scala.util._
import akka.http.javadsl.model
import akka.http.scaladsl.unmarshalling.Unmarshal
import org.json4s.JsonAST._

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.concurrent.duration.Duration._
import scala.concurrent.ExecutionContext.Implicits.global
import collection.mutable
import scala.xml.Utility._
import scala.xml._
import akka.pattern._
import akka.util.Timeout
import java.net._

import Jsoner.Jsoner

object HttpReq{
  def get(url: String,
              connectTimeout: Int = 5000,
              readTimeout: Int = 5000) =
  {
    val requestMethod = "GET"
    import java.net.{URL, HttpURLConnection}
    val connection = new URL(url).openConnection.asInstanceOf[HttpURLConnection]
    connection.setConnectTimeout(connectTimeout)
    connection.setReadTimeout(readTimeout)
    connection.setRequestMethod(requestMethod)
    val inputStream = connection.getInputStream
    val content = io.Source.fromInputStream(inputStream).mkString
    if (inputStream != null) inputStream.close()
    content
  }
  def post(url: String,
          connectTimeout: Int = 5000,
          readTimeout: Int = 5000) =
  {
    val requestMethod = "POST"
    import java.net.{URL, HttpURLConnection}
    val connection = new URL(url).openConnection.asInstanceOf[HttpURLConnection]
    connection.setConnectTimeout(connectTimeout)
    connection.setReadTimeout(readTimeout)
    connection.setRequestMethod(requestMethod)
    val inputStream = connection.getInputStream
    val content = io.Source.fromInputStream(inputStream).mkString
    if (inputStream != null) inputStream.close()
    content
  }
}

object KeyHandler {
  def privateKey(id : String) = {
    "42"
  }
  def publicKey(id : String) = {
    "23"
  }
  def generatePair(id : String) = ???
}

object Cipherer {
  def decryptMessage(message : String, from : String) = {
    val keyToUse = KeyHandler.privateKey(from)
    message
  }
  def cryptMessage(message : String, to : String) = {
    val keyToUse = KeyHandler.publicKey(to)
    message
  }
}

object Main extends App {
  def locals(from: Int, to: Int, ipAddress : List[Int]): List[List[Int]] = {
    if (from > to) List()
    else
      ipAddress.updated(3, from) :: locals(from + 1, to, ipAddress)
  }
  implicit val system = ActorSystem("my-system")
  implicit val dispatcher = system.dispatcher
  implicit val materializer = ActorMaterializer()
  val localIpAddress = InetAddress.getLocalHost.getAddress.map(b => (b.toInt + 256) % 256).toList
  val route =
    pathPrefix("askForKey") {
      pathEnd {
        complete(HttpEntity(ContentTypes.`application/json`, s"""{"key":${KeyHandler.privateKey("someone")}"""))
      }
    } ~
      pathPrefix("findSomeone") {
        pathEnd {
          val allLocals = locals(0, 255, localIpAddress).map(l => l.mkString("http://", ".", "/askForKey"))
          val allReqs = allLocals.map(l => Future {HttpReq.get(l)})
          complete("")
        }
      }
  val bindingFuture = Http().bindAndHandle(route, "localhost", 8080)
  println(localIpAddress.mkString("http://", ".", "/askForKey"))
  println(InetAddress.getLocalHost.getHostAddress)
  //println(HttpReq.get(localIpAddress.mkString("http://", ".", ":8080/askForKey")))
}