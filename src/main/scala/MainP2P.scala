import java.net._

import org.json4s.jackson.JsonMethods.{compact, parse, render}
import org.json4s.JsonDSL._
import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, Props, Status}
import akka.http.scaladsl.server.{Route, StandardRoute}
import akka.http.scaladsl.{Http, server}
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import akka.stream.ActorMaterializer
import akka.http.scaladsl.server.Directives
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport

import scala.xml._
import scala.xml.Utility.trim
import scala.io.StdIn
import scala.util._
import akka.http.javadsl.model
import akka.http.scaladsl.unmarshalling.Unmarshal
import org.json4s.JsonAST._
import spray.json._

import scala.concurrent.{Await, Future}
import MyCiphering._
import org.json4s.DefaultFormats
import spray.json.DefaultJsonProtocol
import akka.pattern._

import scala.concurrent.duration.{Duration, MILLISECONDS}
import akka.actor._

import scala.concurrent.duration._
import scala.concurrent.duration
import collection.mutable
import Jsoner._
import akka.util.Timeout
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

case object FindSomeone
case class FoundSomeone(that : ActorRef)

case class TryMatch(enc : EncryptedVector)

object TinderActor {
  def props(interests : Array[Boolean], finder : ActorRef) =
    Props(classOf[TinderActor], interests, finder)
}

class TinderActor(interests : Array[Boolean], finder : ActorRef) extends Actor with ActorLogging {
  val openKeys = mutable.Map[ActorRef, PublicKey]()
  val closeKeys = mutable.Map[ActorRef, PrivateKey]()
  def interestsToBigInt = interests.map(x => if(x) BigInt(1) else BigInt(0))
  implicit val timeout = Timeout(1 second)
  override def receive: Receive = {
    case FoundSomeone(that) => {
      val keys = Paillier.generateKeys()
      openKeys.update(that, keys.publicKey)
      closeKeys.update(that, keys.privateKey)
      log.info("Отправляю запрос")
      val rsFuture = that ? TryMatch(SafeScalar.encryptVector(interestsToBigInt, keys.publicKey))
      Try(Await.result(rsFuture, 1 second)) match {
        case Success(rsAny) => {
          rsAny match {
            case rs : RS => {
              val matching = SafeScalar.decryptRS(rs, closeKeys(that), interestsToBigInt)
              log.info(s"Ваш процент совпадения - ${matching.toDouble / interests.length * 100}%")
            }
            case _ => log.info("Оно ответило чем-то не тем")
          }
        }
        case Failure(exception) => log.info(s"Оно не ответило: ${exception.getMessage}")
      }
    }
    case TryMatch(enc) => {
      val rs = SafeScalar.computeRS(enc, interestsToBigInt)
      sender ! rs
    }
  }
}

class TinderFinder() extends Actor with ActorLogging{
  override def receive: Receive = {
    case x => log.info(x.toString)
  }
}

object Main extends App {
  implicit val system = ActorSystem("my-system")
  implicit val dispatcher = system.dispatcher
  implicit val materializer = ActorMaterializer()
  val finder = system.actorOf(Props(new TinderFinder))
  val Masha = system.actorOf(TinderActor.props(Array(false, true, false, true), finder), "Masha")
  val Vasya = system.actorOf(TinderActor.props(Array(false, true, true, false), finder), "Vasya")
  Masha ! FoundSomeone(Vasya)
}