import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, Props, Status}
import akka.stream.ActorMaterializer
import scala.util._

import scala.concurrent._
import MyCiphering._
import akka.pattern._

import scala.concurrent.duration._
import akka.actor._

import scala.concurrent.duration._
import collection.mutable
import akka.util.Timeout
import fillForm.formFill
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
      log.info("Получил запрос на матчинг")
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
  val Masha = system.actorOf(TinderActor.props(formFill().toArray, finder), "Masha")
  val vasyaInterests = Array.fill(14)(Random.nextBoolean())
  println(vasyaInterests.mkString(","))
  val Vasya = system.actorOf(TinderActor.props(vasyaInterests, finder), "Vasya")
  Masha ! FoundSomeone(Vasya)
}