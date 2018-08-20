package Nums
import collection.mutable
import scala.util.Random

object Primer {
  private def eratFalseFill(l : mutable.ArrayBuffer[Boolean], step : Long, x : Long): Unit = {
    if (x >= l.length)
      ()
    else {
      l(x.toInt) = false
      eratFalseFill(l, step, x + step)
    }
  }
  private def eratAllFill(l : mutable.ArrayBuffer[Boolean], x : Long = 2): Unit = {
    if (x >= l.length) ()
    else {
      if (l(x.toInt))
        eratFalseFill(l, x, x * x)
      else ()
      eratAllFill(l, x + 1)
    }
  }
  private def eratFill(n : Long): Array[Boolean] = {
    val l = List.fill((n + 1).toInt)(true).to[mutable.ArrayBuffer]
    eratAllFill(l)
    l.to[Array]
  }
  val primes = eratFill(100000).zipWithIndex.filter(p => p._1).map(p => p._2).drop(5000)
  println(primes.mkString(","))
  val rand = new Random(112)
  def nextPrime() = primes((rand.nextInt() % primes.length).abs)
  def gcd(x : BigInt, y : BigInt): BigInt = {
    if (y == 0) x
    else gcd(y, x % y)
  }
  def lcm(x : BigInt, y : BigInt) = x * y / gcd(x, y)
  def getPQ: (BigInt, BigInt) = {
    val p = nextPrime()
    val q = nextPrime()
    if (gcd(p * q, (p - 1) * (q - 1)) == 1) {
      (p, q)
    }
    else
      getPQ
  }
  def binPowMod(x : BigInt, n : BigInt, m : BigInt): BigInt = {
    if (n == 0) 1
    else {
      if (n % 2 == 0) {
        val t = binPowMod(x, n / 2, m)
        (t * t) % m
      }
      else (binPowMod(x, n - 1, m) * x) % m
    }
  }
  def gcdSolve (a : BigInt, b : BigInt): ((BigInt, BigInt), BigInt) = {
    if (a == 0) ((0, 1), b)
    else {
      val t = gcdSolve (b%a, a)
      val x = t._1._1
      val y = t._1._2
      ((y - (b / a) * x, x), t._2)
    }
  }
}

case class PublicKey(n : BigInt, g : BigInt)
case class PrivateKey(l : BigInt, u : BigInt, n : BigInt)
case class PaillierKeys(privateKey : PrivateKey, publicKey: PublicKey)

object Paillier {
  def modInvert(a : BigInt, m : BigInt): BigInt = {
    val t = Primer.gcdSolve(a, m)
    t._1._1
  }
  val rand = new Random()
  def generateKeys() = {
    val pq = Primer.getPQ
    println(pq)
    val n = pq._1 * pq._2
    val l = Primer.lcm(pq._1 - 1, pq._2 - 1)
    val g = BigInt(rand.nextLong().abs) % n
    val u = (modInvert(
      (Primer.binPowMod(g, l, n * n) - 1) / n
      , n) % n + n) % n
    PaillierKeys(PrivateKey(l, u, n), PublicKey(n, g))
  }
  def encrypt(k : PublicKey, m : BigInt) = {
    val r = BigInt(rand.nextLong().abs) % k.n
    val gm = Primer.binPowMod(k.g, m, k.n * k.n)
    val rn = Primer.binPowMod(r, k.n, k.n * k.n)
    (gm * rn) % (k.n * k.n)
  }
  def decrypt(k : PrivateKey, c : BigInt) =
    ((((Primer.binPowMod(c, k.l, k.n * k.n) - 1) / k.n) * k.u) % k.n + k.n) % k.n
}

case class EncryptedVector(l : Array[Int])

object SafeScalar {
  val rand = new Random(789)
  def encryptVector(ar : Array[BigInt], publicKey: PublicKey) =
    ar.map(x => Paillier.encrypt(publicKey, x))
  def computeRS(encAr : Array[BigInt], publicKey: PublicKey) = {
    val pi1 = rand.shuffle(encAr.indices)
  }
}

object NumTester extends App {
  val keys = Paillier.generateKeys()
  println(keys.privateKey)
  println(keys.publicKey)
  val m = 17823
  val t = 731928
  println(m)
  println(t)
  val cipheredM = Paillier.encrypt(keys.publicKey, m)
  val cipheredT = Paillier.encrypt(keys.publicKey, t)
  println(Paillier.decrypt(keys.privateKey, cipheredM))
  println(Paillier.decrypt(keys.privateKey, cipheredT))
  println(Paillier.decrypt(keys.privateKey, cipheredM * cipheredT))
}