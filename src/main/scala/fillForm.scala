import scala.collection.mutable.ListBuffer
import scala.io.StdIn._
object fillForm {
  def formFill() = {
    println("Заполните анкету: введитите 1, если вы приветсттвуете это во взаимооттношениях/итересуетесь этим, и 0, если нет")
    val items = scala.io.Source.fromFile("/Users/daniiltekunov/IdeaProjects/Playground/src/main/scala/form.txt").getLines
    val vect = ListBuffer[Boolean]()
    items.foreach(subject => {
      println(subject)
      vect += readBoolean()
    })
    vect
  }
}
