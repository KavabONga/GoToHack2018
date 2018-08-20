package Paillier
import collection.mutable
import scala.util.Random

object Paillier {
  private def eratFalseFill(l : mutable.ArrayBuffer[Boolean], step : Int, x : Int): Unit = {
    if (x >= l.length)
      ()
    else {
      l(x) = false
      eratFalseFill(l, step, x + step)
    }
  }
  private def eratAllFill(l : mutable.ArrayBuffer[Boolean], x : Int = 2): Unit = {
    if (x >= l.length) ()
    else {
      if (l(x))
        eratFalseFill(l, x, x * x)
      else ()
      eratAllFill(l, x + 1)
    }
  }
  private def eratFill(n : Int): Array[Boolean] = {
    val l = List.fill(n + 1)(true).to[mutable.ArrayBuffer]
    eratAllFill(l)
    l.to[Array]
  }
  val primes = eratFill(10000).zipWithIndex.filter(p => p._1).map(p => p._2).drop(2)
  val rand = new Random(112)
  def getPQ() = ???
}
