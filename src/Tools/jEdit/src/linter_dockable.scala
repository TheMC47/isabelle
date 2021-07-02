package isabelle.jedit

import isabelle._
import isabelle.jedit_base.Dockable
import org.gjt.sp.jedit.View
import scala.swing.{TextArea, ScrollPane}
import linter._

object Linter_Dockable {

  private def do_replace(
      doc_view: Document_View,
      range: Text.Range,
      content: String
  ): Unit = {
    val text_area = doc_view.text_area
    val buffer = text_area.getBuffer
    JEdit_Lib.buffer_edit(buffer) {
      buffer.remove(range.start, range.length)
      text_area.moveCaretPosition(range.start)
      text_area.setSelectedText(content)
    }
  }

  class Handler extends Active.Handler {
    override def handle(
        view: View,
        text: String,
        elem: XML.Elem,
        doc_view: Document_View,
        snapshot: Document.Snapshot
    ): Boolean = {
      elem match {
        case XML.Elem(Markup(Markup.LINTER, props), body) =>
          for {
            range <- Position.Range.unapply(props)
            content <- Markup.Content.unapply(props)
          } do_replace(doc_view, range, content)
          true
        case _ => false
      }
    }
  }

}

class Linter_Dockable(view: View, position: String)
    extends Dockable(view, position) {

  private var current_output: List[XML.Tree] = Nil

  val output = new Pretty_Text_Area(view)
  set_content(output)

  val separator = XML_Lint_Reporter.text("----------------")
  val disabled = XML_Lint_Reporter.text("The linter plugin is disabled.")

  override def detach_operation: Option[() => Unit] = output.detach_operation

  def lint(): Unit = {

    GUI_Thread.require {}

    for {
      snapshot <- PIDE.maybe_snapshot(view)
      if !snapshot.is_outdated
    } {
      val new_output = PIDE.plugin.linter.get match {
        case None => disabled
        case Some(linter) =>
          val lint_all = PIDE.options.bool("lint_buffer")
          val current_command =
            PIDE.editor.current_command(view, snapshot) match {
              case None          => Command.empty
              case Some(command) => command
            }

          val command_lints = linter
            .lint_report(snapshot)
            .command_lints(current_command.id) match {
            case Nil => XML_Lint_Reporter.text("No lints here.")
            case lint_results @ (_ :: _) =>
              XML_Lint_Reporter.report_lints(
                lint_results,
                print_location = false,
                print_name = false
              )
          }

          val all_lints = if (lint_all) {
            val all_results = linter
              .lint_report(snapshot)
              .results
            separator ::: XML_Lint_Reporter.report_lints(
              all_results,
              print_location = true,
              print_name = true
            )
          } else Nil
          command_lints ::: all_lints
      }

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
