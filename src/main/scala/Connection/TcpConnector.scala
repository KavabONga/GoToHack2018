package hack

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.io.{IO, Tcp}
import akka.util.ByteString
import java.net.InetSocketAddress

import hack.common.{Serialization}

import scala.collection.mutable.ArrayBuffer

case class MessageReceived(from : ActorRef, message : Any)
case class MessageSend(to : ActorRef, message : Any)

/**
  * A simplistic Actor for initiating TCP client connections
  *
  * Deserialises all the incoming data to Scala objects and sends to the parent
  */
class TcpClient(remote: InetSocketAddress) extends Actor with ActorLogging {

  import Tcp._
  import context.system
  IO(Tcp) ! Connect(remote)

  def receive = {
    case c @ CommandFailed(con : Connect) ⇒ {
      log.info("Что-то провалилось")
      context stop self
      context.parent ! MessageReceived(sender, c)
    }
    case con @ Connected(remote, local) ⇒ {
      log.info(s"Соединился с $remote")
      context.parent ! MessageReceived(sender, con)
      val connection = sender
      connection ! Register(self)
      context become {
        case Received(data) => {
          log.info(s"Получил ${Serialization.deserialise(data)}")
          context.parent ! MessageReceived(sender, Serialization.deserialise(data))
        }
        case MessageSend(to, message) => {
          log.info(s"Отправляю $message")
          to ! Write(Serialization.serialise(message))
        }
        case c : ConnectionClosed => {
          log.info(s"Закрываюсь, $c")
          context.parent ! MessageReceived(sender, c)
          context stop self
        }
        case x =>
          log.info(s"Получил что-то странное: $x")
      }
    }
    case x =>
      log.info(s"Получил что-то странное: $x")
  }
}

object TcpClient {
  def props(remote: InetSocketAddress) =
    Props(classOf[TcpClient], remote)
}

/**
  * A simplistic Actor for listening to TCP connections
  *
  * Deserialises all the incoming data to Scala objects and sends to the parent
  * Sends the same message to all the clients it's connected to
  */

class TcpServer(local : InetSocketAddress) extends Actor with ActorLogging{

  import Tcp._
  import context.system

  IO(Tcp) ! Bind(self, local)

  val connections = ArrayBuffer.empty[ActorRef]

  def receive = {
    case b @ Bound(localAddress) ⇒ {
      context.parent ! MessageReceived(sender, b)
      context become {
        case c @ Connected(remote, _) => {
          log.info(s"Соединился с $remote")
          context.parent ! MessageReceived(sender, c)
          sender ! Register(self)
          connections.append(sender)
        }
        case Received(data) => {
          log.info(s"Получил ${Serialization.deserialise(data)}")
          context.parent ! MessageReceived(sender, Serialization.deserialise(data))
        }
        case MessageSend(to, message) => {
          log.info(s"Отправляю $message всем")
          to ! Write(Serialization.serialise(message))
        }
        case c: ConnectionClosed => {
          log.info(s"Закрываюсь, $c")
          context.parent ! MessageReceived(sender, c)
          context stop self
        }
        case x =>
          log.info(s"Получил что-то странное: $x")
      }
    }

    case c @ CommandFailed(_: Bind) ⇒ {
      log.info("Что-то провалилось")
      context.parent ! MessageReceived(sender, c)
      context stop self
    }
    case x =>
      log.info(s"Получил что-то странное: $x")
  }
}

object TcpServer {
  def props(local : InetSocketAddress) =
    Props(classOf[TcpServer], local)
}