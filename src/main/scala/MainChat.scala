import java.net.InetSocketAddress

import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, Props, Status}

import scala.util._
import hack._
import akka.stream.ActorMaterializer
import akka.actor._

import scala.concurrent.ExecutionContextExecutor

case object MashaVasyaChat

class ChatInitiator(implicit system : ActorSystem, materializer : ActorMaterializer) extends Actor {
  import akka.io.Tcp._
  override def receive = {
    case MashaVasyaChat => {

      val centralAddress = new InetSocketAddress(0)
      println(s"Создаю сервер на $centralAddress...")
      val central = context.actorOf(TinderCentral.props(centralAddress), "central")
    }
    case Bound(centralAddress) => {
      println(s"Создал сервер на $centralAddress")
      val results = (1 to 14).map(_ => Random.nextBoolean()).toArray //Form.formFill()
      println("Создаю Машу...")
      val Masha = context.actorOf(TinderUser.props(centralAddress, results), "Masha")
      println("Создал Машу")
      val vasyaInterests = (1 to results.length).map(_ => Random.nextBoolean()).toArray
      println(vasyaInterests.mkString(","))
      println("Создаю Васю...")
      val Vasya = context.actorOf(TinderUser.props(centralAddress, vasyaInterests), "Vasya")
      println("Создал Васю")
    }
  }
}

object ChatInitiator {
  def props(implicit system: ActorSystem, materializer: ActorMaterializer) =
    Props(new ChatInitiator)
}

object Chat extends App {
  override def main(args: Array[String]): Unit = {
    implicit val system: ActorSystem = ActorSystem("my-system")
    implicit val materializer: ActorMaterializer = ActorMaterializer()
    val chatter = system.actorOf(ChatInitiator.props)
    chatter ! MashaVasyaChat
  }
}