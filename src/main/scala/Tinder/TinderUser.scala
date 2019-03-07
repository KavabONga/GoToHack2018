package hack

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.util.ByteString
import hack.ciphering._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

case class FoundSomeone(address : InetSocketAddress)
case class TryMatch(enc : EncryptedVector)
case class ContactMeHere(address : InetSocketAddress)

class TinderCentral(address : InetSocketAddress) extends Actor with ActorLogging {
  import akka.io.Tcp._
  val server = context.actorOf(TcpServer.props(address), s"${self.path.name}Server")
  val users = ArrayBuffer.empty[ActorRef]
  log.info(s"Я сервер и я лежу на $address")
  def receive = {
    // The server is bound to an address
    case MessageReceived(_, b : Bound) => {
      log.info("Забаундил главный сервер")
      context.parent ! b
    }

    // A user says he is ready to accept matches (So it just sends the FoundSomeone to every existing user)
    case MessageReceived(from, ContactMeHere(address)) => {
      users.foreach(server ! MessageSend(_, FoundSomeone(address)))
      users.append(from)
    }
    case x =>
      log.info(s"Получил что-то непонятное: $x")
  }
}

object TinderCentral {
  def props(address : InetSocketAddress) =
    Props(classOf[TinderCentral], address)
}

class TinderUser(central: InetSocketAddress, interests : Array[Boolean]) extends Actor with ActorLogging {
  import akka.io.Tcp._

  def bigVec = interests.map(x => if(x) BigInt(1) else BigInt(0))

  val openKeys = mutable.Map[ActorRef, PublicKey]()
  val closedKeys = mutable.Map[ActorRef, PrivateKey]()

  val thisServer = context.actorOf(TcpServer.props(new InetSocketAddress(0)))
  log.info(s"Мои интересы: ${interests.mkString("[", ", ", "]")}")

  override def receive = {

    // The user's server is ready, so he can start communicating
    case MessageReceived(_, Bound(thisServerAddress)) => {
      val toServer = context.actorOf(TcpClient.props(central), s"${self.path.name}ToServer")
      log.info(s"Теперь я здесь: $thisServerAddress")
      context become {

        // Got an notification that a new user appeared, should check for a match
        case MessageReceived(from, FoundSomeone(that)) => {
          log.info(s"Создаю соединение... с $that")
          context.actorOf(TcpClient.props(that))
        }

        // The connection with the server is done
        case MessageReceived(from, c : Connected) if sender == toServer => {
          sender ! MessageSend(from, ContactMeHere(thisServerAddress))
        }

        // When someone found you in order to match
        case MessageReceived(from, Connected(_, local)) if sender == thisServer => {
          val keys = Paillier.generateKeys()
          openKeys.update(sender, keys.publicKey)
          closedKeys.update(sender, keys.privateKey)
          log.info("Отправляю запрос на матчинг")
          sender ! MessageSend(from, TryMatch(SafeScalar.encryptVector(bigVec, keys.publicKey)))
        }
        case MessageReceived(from, rs : RS) => {
          val matching = SafeScalar.decryptRS(rs, closedKeys(sender), bigVec)
          log.info(s"Ваш процент совпадения - ${matching.toDouble / interests.length * 100}%")
          from ! Close
        }
        case MessageReceived(from, TryMatch(enc)) => {
          log.info("Получил запрос на матчинг")
          val rs = SafeScalar.computeRS(enc, bigVec)
          sender ! MessageSend(from, rs)
        }
        case MessageReceived(from, c : ConnectionClosed) => {
          log.info("Закрыл соединение")
          openKeys -= sender
          closedKeys -= sender
        }
        case x =>
          log.info(s"Получил что-то непонятное: $x")
      }
    }

  }
}

object TinderUser {
  def props(central : InetSocketAddress, interests : Array[Boolean]) =
    Props(classOf[TinderUser], central, interests)
}