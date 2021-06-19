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

          // Lints for the current command
          val lint_current_command = for {
            command <- PIDE.editor.current_command(view, snapshot)
          } yield linter.command_lints(snapshot, command.id)

          val separator = XML.Text("----------------") :: Nil

          val command_lint: XML.Body = lint_current_command match {
            case Some(Nil) | None =>
              XML.Text("No lints here.") :: Nil
            case Some(lint_results) =>
              lint_results
                .map(report(_, snapshot, false))
                .flatten
          }

          val content =
            if (PIDE.options.bool("lint_buffer"))
              command_lint ::: separator ::: linter
                .lint_results(snapshot)
                .map(report(_, snapshot))
                .flatten
            else command_lint

          val new_output = Pretty.separate(content)
          update_output(snapshot, new_output)
        }
        case None =>
          update_output(
            snapshot,
            Pretty.separate(XML.Text("The linter plugin is disabled") :: Nil)
          )
      }
    }
  }

  private def update_output(
      snapshot: Document.Snapshot,
      new_output: List[XML.Tree]
  ): Unit =
    if (current_output != new_output) {
      output.update(
        snapshot,
        Command.Results.empty,
        Pretty.separate(new_output)
      )
      current_output = new_output
    }

  def report(
      lint_result: Linter.Lint_Result,
      snapshot: Document.Snapshot,
      print_location: Boolean = true
  ): XML.Body = {
    def range_to_line(range: Text.Range): Line.Range = {
      val document = Line.Document(snapshot.node.source)
      document.range(range)
    }

    val text = new StringBuilder()
    if (print_location)
      text ++= s"At ${range_to_line(lint_result.range).start.print}:  "

    text ++= s"[${lint_result.lint_name}]\n"
    text ++= lint_result.message
    val edit = lint_result.edit match {
      case None => Nil
      case Some(edit) =>
        XML.Text("Consider: ") :: XML.Elem(
          Markup(Markup.LINTER, Position.Range(edit.range) ::: Markup.Content(edit.replacement)),
          XML.Text(edit.message) :: Nil
        ) :: Nil
    }
    XML.Text(text.toString()) :: edit
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
