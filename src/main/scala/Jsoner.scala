package Jsoner

import hack.ciphering._
import org.json4s._
import org.json4s.native.Serialization

object Jsoner {
  implicit val formats = DefaultFormats
  def encryptedVectorToJson(enc: EncryptedVector) = {
    val m = JObject(
      "vec" -> enc.l,
      "key" -> JObject(
        "n" -> enc.publicKey.n,
        "g" -> enc.publicKey.g
      ),
      "s" -> enc.s
    )
    Serialization.write(m)
  }
  def RSToJson(rs : RS) = {
    val m = JObject(
      "S" -> rs.S,
      "R" -> rs.R,
      "S1" -> rs.S1,
      "R1" -> rs.R1,
      "s" -> rs.s
    )
    m
    Serialization.write(m)
  }
}
