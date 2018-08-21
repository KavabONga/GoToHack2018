import scala.collection.mutable.ListBuffer
import scala.io.StdIn._
import scala.util._
object fillForm {
  def formFill() = {
    println("Заполните анкету: введите 1, если вы приветствуете это во взаимоотношениях/итересуетесь этим или 0, если нет")
    val items = scala.io.Source.fromFile("C:\\Users\\mitya\\Documents\\Git_workspace\\GoToHack2018\\src\\main\\scala\\form.txt").getLines
    val vect = ListBuffer[Boolean]()
    items.foreach(subject => {
      println(subject)
      val inp = readLine()
      vect += {Try(inp.toInt) match {
        case Success(value) if value > 0 => true
        case _ => false
      }}
    })
    vect
  }
}
