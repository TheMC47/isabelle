package isabelle.jedit

import isabelle._
import isabelle.jedit_base.Dockable
import org.gjt.sp.jedit.View
import scala.swing.{TextArea, ScrollPane}
import linter._

class Linter_Dockable(view: View, position: String)
    extends Dockable(view, position) {

  private var current_output: List[XML.Tree] = Nil

  val output = new Pretty_Text_Area(view)
  set_content(output)

  override def detach_operation: Option[() => Unit] = output.detach_operation

  def lint(): Unit = {

    GUI_Thread.require {}

    for {
      snapshot <- PIDE.maybe_snapshot(view)
      if !snapshot.is_outdated
    } {
      val version = snapshot.version
      val state = snapshot.state
      PIDE.plugin.linter.get match {
        case Some(linter) => {
          val content = linter.lint_results(snapshot)
            .map(report(_, snapshot))
            .mkString("\n\n-------------------------------\n")
          print(snapshot, s"$snapshot \n$state \n\n$content")
        }
        case None =>
          print(snapshot, s"The linter plugin is disabled")
      }
    }
  }

  private def print(snapshot: Document.Snapshot, content: String): Unit = {
    val new_output = Pretty.separate(XML.Text(content) :: Nil)
    if (current_output != new_output) {
      output.update(
        snapshot,
        Command.Results.empty,
        Pretty.separate(new_output)
      )
      current_output = new_output
    }
  }

  def report(lint_report: Linter.Lint_Result, snapshot: Document.Snapshot): String = {
    def range_to_line(range: Text.Range): Line.Range = {
      val document = Line.Document(snapshot.node.source)
      document.range(range)
    }

    val result = new StringBuilder()
    result ++= s"At ${range_to_line(lint_report.range).start.print}:   [${lint_report.lint_name}]\n\n"
    result ++= lint_report.message
    val edit = lint_report.edit match {
      case None => ""
      case Some((before, after)) =>
        s"""\nConsider changing "$before" to "$after""""
    }
    result ++= edit
    result.toString()
  }

  /* main */

  private val main =
    Session.Consumer[Any](getClass.getName) { _ =>
      GUI_Thread.later { lint() }
    }

  override def init(): Unit = {
    PIDE.session.global_options += main
    PIDE.session.commands_changed += main
    PIDE.session.caret_focus += main
  }

  override def exit(): Unit = {
    PIDE.session.global_options -= main
    PIDE.session.commands_changed -= main
    PIDE.session.caret_focus -= main
  }
}
