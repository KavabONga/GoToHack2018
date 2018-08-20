package Jsoner

import org.json4s.{DefaultFormats, JsonDSL}
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization
import org.json4s.JsonAST.JValue

import collection.mutable.ListBuffer

import MyCiphering._
object Jsoner {
  implicit val formats = DefaultFormats
  def encryptedVectorToJson(enc: EncryptedVector) = {
    val m = Map(
      "vec" -> enc.l,
      "key" -> Map(
        "n" -> enc.publicKey.n,
        "g" -> enc.publicKey.g
      ),
      "s" -> enc.s
    )
    Serialization.write(m)
  }
  def RSToJson(rs : RS) = {
    val m = Map(
      "S" -> rs.S,
      "R" -> rs.R,
      "S1" -> rs.S1,
      "R1" -> rs.R1,
      "s" -> rs.s
    )
    Serialization.write(m)
  }
}
