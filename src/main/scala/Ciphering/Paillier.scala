package hack.ciphering
import collection.mutable
import scala.annotation.tailrec
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
    //println(n)
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

case class EncryptedVector(l : Array[BigInt], s : Int, publicKey: PublicKey)
case class RS(R : Array[BigInt], R1 : Array[BigInt], S : BigInt, S1 : BigInt, s : Int)

object SafeScalar {
  val rand = new Random(789)
  def encryptVector(a : Array[BigInt], publicKey: PublicKey) = {
    val s = rand.nextInt().abs % 100 + 100
    EncryptedVector(a.map(x => Paillier.encrypt(publicKey, x + s)), s, publicKey)
  }
  def computeRS(encAr : EncryptedVector, b : Array[BigInt]) = {
    val pi = rand.shuffle(encAr.l.indices.toList)
    val pi1 = rand.shuffle(encAr.l.indices.toList)
    val r = List.fill(encAr.l.length)(rand.nextInt() % 100)
    val R = encAr.l.indices.toArray.map(i => encAr.l(pi(i)) * Paillier.encrypt(encAr.publicKey, encAr.s - r(pi(i)) - b(pi(i))))
    val R1 = encAr.l.indices.toArray.map(i => encAr.l(pi1(i)) * Paillier.encrypt(encAr.publicKey, encAr.s - r(pi1(i))))
    val S = Paillier.encrypt(encAr.publicKey, encAr.l.indices.map(i => (r(i) + b(i)) * (r(i) + b(i))).sum)
    val S1 = Paillier.encrypt(encAr.publicKey, r.map(x => x * x).sum)
    RS(R, R1, S, S1, encAr.s)
  }
  def decryptRS(rs : RS, privateKey: PrivateKey, a : Array[BigInt]) = {
    val T = a.map(x => x * x).sum
    val U = -rs.R.map(x => Paillier.decrypt(privateKey, x) - 2 * rs.s).map(x => x * x).sum
    val U1 = -rs.R1.map(x => Paillier.decrypt(privateKey, x) - 2 * rs.s).map(x => x * x).sum
    val P = Paillier.decrypt(privateKey, rs.S) + T + U
    val P1 = Paillier.decrypt(privateKey, rs.S1) + T + U1
    (P - P1) / 2
  }
}

object NumTester extends App {
  val keys = Paillier.generateKeys()
  println("Private key: " + keys.privateKey.toString)
  println("Public key: " + keys.publicKey.toString)
  val a = Array(0, 1, 1, 1, 0).map(BigInt(_))
  val b = Array(1, 0, 1, 1, 0).map(BigInt(_))
  val enc = SafeScalar.encryptVector(a, keys.publicKey)
  val compRS = SafeScalar.computeRS(enc, b)
  val scalarRes = SafeScalar.decryptRS(compRS, keys.privateKey, a)
  println(scalarRes)
}