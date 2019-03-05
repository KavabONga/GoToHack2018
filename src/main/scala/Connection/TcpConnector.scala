package hack

import akka.actor.{ Actor, ActorRef, Props }
import akka.io.{ IO, Tcp }
import akka.util.ByteString
import java.net.InetSocketAddress

case class MessageReceived(from : ActorRef, message : Any)
case class MessageSend(to: ActorRef, message : Any)

/**
  * A simplicstic Actor for initiating TCP connections
  *
  * Deserialises all the incoming data to Scala objects and sends to the parent
  */
class TcpClient(remote: InetSocketAddress, local: InetSocketAddress) extends Actor {

  import Tcp._
  import context.system
  IO(Tcp) ! Connect(remote, Some(local))

  def receive = {
    case c @ CommandFailed(_: Connect) ⇒
      context.parent ! c
      context stop self
    case c : Connected ⇒ {
      context.parent ! c
      val connection = sender()
      context become {
        case Received(data) => {
          context.parent ! MessageReceived(connection, Serialization.deserialise(data))
        }
        case MessageSend(to, message) =>
          if (connection == to)
            connection ! Write(Serialization.serialise(message))
          else
            sender ! "Wrong connection"
        case c : ConnectionClosed => {
          context.parent ! c
          context stop self
        }
      }
    }
  }

}

class TcpServer(local : InetSocketAddress) extends Actor {

  import Tcp._
  import context.system

  IO(Tcp) ! Bind(self, local)

  def receive = {
    case b @ Bound(localAddress) ⇒ {
      context.parent ! b
      context become {
        case Connected(remote, local) =>
          sender ! Register(self)
        case Received(data) => {
          context.parent ! MessageReceived(sender, Serialization.deserialise(data))
        }
        case MessageSend(to, message) =>
          to ! Write(Serialization.serialise(message))
        case c: ConnectionClosed => {
          context.parent ! c
          context stop self
        }
      }
    }

    case CommandFailed(_: Bind) ⇒ context stop self

    case c @ Connected(remote, local) ⇒
      val connection = sender()

  }
}