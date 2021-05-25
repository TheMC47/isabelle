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
      val results = Linter.lint(snapshot, get_lints())
      val content = results
        .map(report(_))
        .mkString("\n\n-------------------------------\n")

      val text = XML.Text(content)
      val new_output = Pretty.separate(text :: Nil)
      if (current_output != new_output) {
        output.update(
          snapshot,
          Command.Results.empty,
          Pretty.separate(new_output)
        )
        current_output = new_output
      }
    }
  }

  def report(lint_report: Linter.Lint_Report): String = {
    val result = new StringBuilder()
    result ++= s"At ${lint_report.range.start.print}:   [${lint_report.lint_name}]\n\n"
    result ++= lint_report.message
    val edit = lint_report.edit match {
      case None => ""
      case Some((before, after)) =>
        s"""\nConsider changing "$before" to "$after""""
    }
    result ++= edit
    result.toString()
  }

  def lint_current_command(): Unit = {

    GUI_Thread.require {}

    for {
      snapshot <- PIDE.maybe_snapshot(view)
      if !snapshot.is_outdated
    } {
      val lint_result = for {
        command <- PIDE.editor.current_command(view, snapshot)
      } yield Linter.lint_command(command, snapshot, get_lints())

    }
  }

  def get_lints(): List[Linter.Lint] = Linter.all_lints
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
