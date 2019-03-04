import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, Props, Status}

import scala.util._
import hack._
import akka.stream.ActorMaterializer
import akka.actor._

import scala.concurrent.ExecutionContextExecutor

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



object Chat extends App {
  override def main(args: Array[String]): Unit = {
    implicit val system: ActorSystem = ActorSystem("my-system")
    implicit val materializer: ActorMaterializer = ActorMaterializer()
    val results = Form.formFill()
    val Masha = system.actorOf(Props(classOf[TinderUser], results), "Masha")
    val vasyaInterests = (1 to results.length).map(_ => Random.nextBoolean()).toArray
    println(vasyaInterests.mkString(","))
    val Vasya = system.actorOf(Props(classOf[TinderUser], vasyaInterests), "Vasya")
    println
    Masha ! FoundSomeone(Vasya)
  }
}