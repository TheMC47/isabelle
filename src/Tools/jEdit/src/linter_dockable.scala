package isabelle.jedit

import isabelle._
import isabelle.jedit_base.Dockable
import org.gjt.sp.jedit.View
import scala.swing.{TextArea, ScrollPane}
import linter._

class Linter_Dockable(view: View, position: String)
    extends Dockable(view, position) {

  private val text_area = new TextArea
  set_content(new ScrollPane(text_area))

  def lint(): Unit = {

    GUI_Thread.require {}

    for {
      snapshot <- PIDE.maybe_snapshot(view)
      if !snapshot.is_outdated
    } {
      val results = Linter.lint(snapshot, get_lints())
      report_lint_results(results)
    }
  }

  def report_lint_results(results: List[Linter.Lint_Report]): Unit = {
    text_area.append(results.mkString("\n") + "\n\n\n")
  }

  def parse_current_command(): Unit = {

    GUI_Thread.require {}

    for {
      snapshot <- PIDE.maybe_snapshot(view)
      if !snapshot.is_outdated
    } {
      val command = PIDE.editor.current_command(view, snapshot) match {
        case Some(command) => command
        case None          => Command.empty
      }
      val parsed = Linter.parse_command(command)

      text_area.append(s"$parsed" + "\n\n\n")

    }
  }

  def get_lints(): List[Linter.Lint] = List(Linter.Print_Structure)
  /* main */

  private val main =
    Session.Consumer[Any](getClass.getName) { _ =>
      GUI_Thread.later { parse_current_command() }
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
