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
  val users = ArrayBuffer.empty[ActorRef]
  val server = context.actorOf(TcpServer.props(address), s"${self.path.name}Server")
  log.info(s"Я сервер и я лежу на $address")
  def receive = {
    case MessageReceived(b : Bound) => {
      log.info("Забаундил главный сервер")
      context.parent ! b
    }
    case MessageReceivedFrom(from, Connected(remote, local)) => {
      log.info(s"Узнал о новом юзвере на $remote")
    }
    case MessageReceivedFrom(from, ContactMeHere(address)) => {
      log.info(s"Юзверь готов принимать матчинги. Отправляю известия о нём всем ${users.length} юзверям")
      users.foreach(server ! MessageSendTo(_, FoundSomeone(address)))
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

  val openKeys = mutable.Map[ActorRef, PublicKey]()
  val closedKeys = mutable.Map[ActorRef, PrivateKey]()
  val serverAddress = new InetSocketAddress(0)
  context.actorOf(TcpClient.props(central), s"${self.path.name}ToServer")
  context.actorOf(TcpServer.props(new InetSocketAddress(0)))
  log.info(s"Инициирую соединение с главным сервером ...")
  def bigVec = interests.map(x => if(x) BigInt(1) else BigInt(0))

  override def receive = {
    case MessageReceived(Bound(thisServer)) => {
      log.info(s"Теперь я здесь: $thisServer")
      context become {
        case MessageReceived(FoundSomeone(that)) => {
          log.info(s"Создаю соединение... с $that")
          context.actorOf(TcpClient.props(that))
        }
        case MessageReceived(Connected(c, _)) if c == central => {
          log.info("Присоединился таки к главному серверу")
          sender ! MessageSend(ContactMeHere(thisServer))
        }
        case MessageReceivedFrom(from, Connected(_, _)) => {
          val keys = Paillier.generateKeys()
          openKeys.update(sender, keys.publicKey)
          closedKeys.update(sender, keys.privateKey)
          log.info("Отправляю запрос на матчинг")
          sender ! MessageSendTo(from, TryMatch(SafeScalar.encryptVector(bigVec, keys.publicKey)))
        }
        case MessageReceivedFrom(from, rs : RS) => {
          val matching = SafeScalar.decryptRS(rs, closedKeys(sender), bigVec)
          log.info(s"Ваш процент совпадения - ${matching.toDouble / interests.length * 100}%")
          sender ! Close
        }
        case MessageReceived(TryMatch(enc)) => {
          log.info("Получил запрос на матчинг")
          val rs = SafeScalar.computeRS(enc, bigVec)
          sender ! MessageSend(rs)
        }
        case MessageReceived(c : ConnectionClosed) => {
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