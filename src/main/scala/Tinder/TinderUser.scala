package hack

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.util.ByteString
import hack.ciphering._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

case object NewUser
case class FoundSomeone(that : ActorRef)
case class TryMatch(enc : EncryptedVector)

class TinderCentral extends Actor {
  val users: ArrayBuffer[ActorRef] = ArrayBuffer.empty[ActorRef]
  val server = context.actorOf(Props(classOf[TcpServer], new InetSocketAddress("local", 0)))
  def receive = {
    case MessageReceived(from, message) => {
      users.foreach(_ ! FoundSomeone(from))
      users.append(from)
    }
    case ConnectionClosed
  }
}

class TinderUser(central: InetSocketAddress, interests : Array[Boolean]) extends Actor with ActorLogging {
  val openKeys = mutable.Map[ActorRef, PublicKey]()
  val closedKeys = mutable.Map[ActorRef, PrivateKey]()


  def bigVec = interests.map(x => if(x) BigInt(1) else BigInt(0))

  override def receive = {
    case FoundSomeone(that) => {
      val keys = Paillier.generateKeys()
      openKeys.update(that, keys.publicKey)
      closedKeys.update(that, keys.privateKey)
      log.info("Отправляю запрос")
      that ! TryMatch(SafeScalar.encryptVector(bigVec, keys.publicKey))
    }
    case rs : RS => {
      val matching = SafeScalar.decryptRS(rs, closedKeys(sender), bigVec)
      log.info(s"Ваш процент совпадения - ${matching.toDouble / interests.length * 100}%")
    }
    case TryMatch(enc) => {
      log.info("Получил запрос на матчинг")
      val rs = SafeScalar.computeRS(enc, bigVec)
      sender ! rs
    } 
  }
}
