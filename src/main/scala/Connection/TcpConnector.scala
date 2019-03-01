package hack

import akka.actor.{ Actor, ActorRef, Props }
import akka.io.{ IO, Tcp }
import akka.util.ByteString
import java.net.InetSocketAddress

class TcpClient(remote: InetSocketAddress, listener: ActorRef) extends Actor {

  import Tcp._
  import context.system

  IO(Tcp) ! Connect(remote)

  def receive = {
    case CommandFailed(_: Connect) ⇒
      listener ! "connect failed"
      context stop self
    case c @ Connected(remote, local) ⇒
      listener ! c
      val connection = sender()
      connection ! Register(self)
      context become {
        case data: ByteString ⇒
          connection ! Write(data)
        case CommandFailed(w: Write) ⇒
          // O/S buffer was full
          listener ! "write failed"
        case Received(data) ⇒
          listener ! data
        case "close" ⇒
          connection ! Close
        case _: ConnectionClosed ⇒
          listener ! "connection closed"
          context stop self
      }
  }

}

class TcpServer extends Actor {

  import Tcp._
  import context.system

  IO(Tcp) ! Bind(self, new InetSocketAddress("localhost", 0))

  def receive = {
    case b @ Bound(localAddress) ⇒
      context.parent ! b

    case CommandFailed(_: Bind) ⇒ context stop self

    case c @ Connected(remote, local) ⇒
      val handler = context.actorOf(Props[TcpDataHandler])
      val connection = sender()
      connection ! Register(handler)
  }

}

class TcpDataHandler extends Actor {
  import Tcp._
  def receive = {
    case Received(data) ⇒ sender() ! Write(data)
    case PeerClosed     ⇒ context stop self
  }
}