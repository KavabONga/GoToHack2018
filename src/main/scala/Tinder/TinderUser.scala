package hack

import akka.actor.{Actor, ActorLogging, ActorRef, Props, ReceiveTimeout}
import akka.util.Timeout
import hack.ciphering._

import scala.concurrent.duration._
import scala.collection.mutable
import scala.concurrent.Await
import scala.util.{Failure, Success, Try}

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
  context.setReceiveTimeout(5 seconds)


  def bigVec = interests.map(x => if(x) BigInt(1) else BigInt(0))

  override def receive = {
    case FoundSomeone(that) => {
      val keys = Paillier.generateKeys()
      openKeys.update(that, keys.publicKey)
      closeKeys.update(that, keys.privateKey)
      log.info("Отправляю запрос")
      that ! TryMatch(SafeScalar.encryptVector(bigVec, keys.publicKey))
    }
    case rs : RS => {
      val matching = SafeScalar.decryptRS(rs, closeKeys(sender), bigVec)
      log.info(s"Ваш процент совпадения - ${matching.toDouble / interests.length * 100}%")
    }
    case TryMatch(enc) => {
      log.info("Получил запрос на матчинг")
      val rs = SafeScalar.computeRS(enc, bigVec)
      sender ! rs
    }
  }
}

class TinderFinder() extends Actor with ActorLogging {
  override def receive: Receive = {
    case x => log.info(x.toString)
  }
}
