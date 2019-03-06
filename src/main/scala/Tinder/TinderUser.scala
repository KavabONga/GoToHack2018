package hack

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.util.ByteString
import hack.ciphering._
import hack.common.Connection

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

case class FoundSomeone(address : InetSocketAddress)
case class TryMatch(enc : EncryptedVector)

class TinderCentral(address : InetSocketAddress) extends Actor {
  import akka.io.Tcp._

  val users = ArrayBuffer.empty[ActorRef]
  val server = context.actorOf(TcpServer.props(address))
  def receive = {
    case MessageReceived(_, Connected(remote, local)) => {
      users.foreach(_! FoundSomeone(remote))
      users.append(sender)
    }
  }
}

class TinderUser(central: InetSocketAddress, interests : Array[Boolean]) extends Actor with ActorLogging {
  import akka.io.Tcp._

  val openKeys = mutable.Map[Connection, PublicKey]()
  val closedKeys = mutable.Map[Connection, PrivateKey]()
  val serverAddress = new InetSocketAddress("local", 0)
  context.actorOf(TcpClient.props(central, new InetSocketAddress("local", 0)))

  def bigVec = interests.map(x => if(x) BigInt(1) else BigInt(0))

  override def receive = {

    case MessageReceived(from, FoundSomeone(that)) => {
      context.actorOf(TcpClient.props(that, new InetSocketAddress("local", 0)))
    }
    case MessageReceived(from, Connected(_, _)) => {
      val keys = Paillier.generateKeys()
      openKeys.update(from, keys.publicKey)
      closedKeys.update(from, keys.privateKey)
      log.info("Отправляю запрос")
      sender ! MessageSend(from, TryMatch(SafeScalar.encryptVector(bigVec, keys.publicKey)))
    }
    case MessageReceived(from, rs : RS) => {
      val matching = SafeScalar.decryptRS(rs, closedKeys(from), bigVec)
      log.info(s"Ваш процент совпадения - ${matching.toDouble / interests.length * 100}%")
      from ! Close
    }
    case MessageReceived(from, TryMatch(enc)) => {
      log.info("Получил запрос на матчинг")
      val rs = SafeScalar.computeRS(enc, bigVec)
    }
    case MessageReceived(from, c : ConnectionClosed) => {
      openKeys -= from
      closedKeys -= from
    }
  }
}
