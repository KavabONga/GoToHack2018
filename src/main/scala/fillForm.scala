package hack

import scala.collection.mutable.ListBuffer
import scala.io.StdIn._
import scala.util._
import scala.io.Source

object Form {
  def formFill(): Array[Boolean] = {
    println("Заполните анкету: введите 1, если вы приветствуете это во взаимоотношениях/итересуетесь этим или 0, если нет")
    val items = Source.fromInputStream(getClass.getClassLoader.getResourceAsStream("form.txt")).getLines().toArray
    println(items)
    val vect = items.map(subject => {
      println(subject)
      readLine() == "1"
    })
    println("Получил все ответы")
    vect
  }
}
