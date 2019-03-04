package hack

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream}

import akka.util.ByteString

object Serialization extends App {
  def serialise(value: Any) = {
    val stream: ByteArrayOutputStream = new ByteArrayOutputStream()
    val oos = new ObjectOutputStream(stream)
    oos.writeObject(value)
    oos.close()
    ByteString(stream.toByteArray)
  }

  def deserialise(bytes: ByteString): Any = {
    val ois = new ObjectInputStream(new ByteArrayInputStream(bytes.toArray))
    val value = ois.readObject
    ois.close()
    value
  }
}