import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, Props, Status}
import scala.util._

import hack._
import akka.stream.ActorMaterializer
import akka.actor._

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
    implicit val system = ActorSystem("my-system")
    implicit val dispatcher = system.dispatcher
    implicit val materializer = ActorMaterializer()
    val finder = system.actorOf(Props(new TinderFinder))
    val results = Form.formFill()
    val Masha = system.actorOf(TinderActor.props(results, finder), "Masha")
    val vasyaInterests = (1 to results.length).map(_ => Random.nextBoolean()).toArray
    println(vasyaInterests.mkString(","))
    val Vasya = system.actorOf(TinderActor.props(vasyaInterests, finder), "Vasya")
    Masha ! FoundSomeone(Vasya)
  }
}