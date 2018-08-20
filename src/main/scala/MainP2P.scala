import org.json4s.jackson.JsonMethods.{compact, parse, render}
import org.json4s.JsonDSL._
import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, Props, Status}
import akka.http.scaladsl.server.{Route, StandardRoute}
import akka.http.scaladsl.{Http, server}
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import akka.stream.ActorMaterializer
import org.json4s.{DefaultFormats, JsonDSL}
import org.json4s.jackson.{Json, JsonMethods}
import JsonMethods._

import scala.xml._
import scala.xml.Utility.trim
import scala.io.StdIn
import scala.util._
import akka.http.javadsl.model
import akka.http.scaladsl.unmarshalling.Unmarshal
import org.json4s.JsonAST._

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.concurrent.duration.Duration._
import scala.concurrent.ExecutionContext.Implicits.global
import collection.mutable
import scala.xml.Utility._
import scala.xml._
import akka.pattern._
import akka.util.Timeout
import Paillier.Paillier
import java.net._

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


object Main extends App {
  def locals(from: Int, to: Int, ipAddress : Array[Byte]): List[Array[Byte]] = {
    if (from > to) List()
    else
      ipAddress.updated(3, from.toByte) :: locals(from + 1, to, ipAddress)
  }
  implicit val system = ActorSystem("my-system")
  implicit val dispatcher = system.dispatcher
  implicit val materializer = ActorMaterializer()
  val localIpAddress = InetAddress.getLocalHost.getAddress
  val route =
    pathPrefix("askForKey") {
      pathEnd {
        val localAdresses = locals(1, 20, localIpAddress)
        complete(localAdresses.map(ip => ip.mkString(".")).mkString("\n"))
      }
    }

  val bindingFuture = Http().bindAndHandle(route, "localhost", 8080)
  println(HttpReq.get(localIpAddress.mkString("http://", ".", "/askForKey")))
}