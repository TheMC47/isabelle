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

  override def detach_operation: Option[() => Unit] = output.detach_operation

  def lint(): Unit = {

    GUI_Thread.require {}

    for {
      snapshot <- PIDE.maybe_snapshot(view)
      if !snapshot.is_outdated
    } {
      val lint_all = PIDE.options.bool("lint_buffer")
      val current_command = PIDE.editor.current_command(view, snapshot) match {
        case None          => Command.empty
        case Some(command) => command
      }

      val new_output = XML_Lint_Reporter.report_snapshot_xml(
        current_command,
        snapshot,
        lint_all = lint_all,
        print_disabled = true,
        print_empty = true,
        print_name =  true
      )

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

object XML_Lint_Reporter {

  // TODO move me

  val HEADER = XML.elem(Markup.KEYWORD1, text("lints: ")) :: Nil
  val EMPTY = text("No lints here.")
  val SEPARATOR = text("----------------")
  val DISABLED = text("The linter plugin is disabled.")

  def report_snapshot_xml(
      current_command: Command,
      snapshot: Document.Snapshot,
      lint_all: Boolean = false,
      print_header: Boolean = false,
      print_empty: Boolean = false,
      print_disabled: Boolean = false,
      print_name: Boolean = false
  ): XML.Body = {
    val header = when(print_header, HEADER)
    val empty = when(print_empty, EMPTY)
    val disabled = when(print_disabled, DISABLED)

    PIDE.plugin.linter.get match {
      case None => disabled
      case Some(linter) =>
        val command_lint =
          linter.lint_report(snapshot).command_lints(current_command.id) match {
            case Nil => empty
            case lint_results @ (_ :: _) =>
              report_lints_xml(
                lint_results,
                snapshot,
                print_location = false,
                print_name = print_name
              )
          }
        val all_lints = when(
          lint_all,
          SEPARATOR ::: linter
            .lint_report(snapshot)
            .results
            .map(
              report_lint_xml(
                _,
                snapshot,
                print_location = true,
                print_name = print_name
              )
            )
            .flatten
        )
        val lints = command_lint ::: all_lints
        if (lints.isEmpty) lints else header ::: lints
    }
  }

  def report_lints_xml(
      lint_results: List[Linter.Lint_Result],
      snapshot: Document.Snapshot,
      print_location: Boolean = false,
      print_name: Boolean = false
  ): XML.Body =
    lint_results.zipWithIndex
      .map(ri =>
        report_lint_xml(
          ri._1,
          snapshot,
          ri._2,
          print_location = print_location,
          print_name = print_name
        )
      )
      .flatten

  def report_lint_xml(
      lint_result: Linter.Lint_Result,
      snapshot: Document.Snapshot,
      lint_number: Int = 0,
      print_location: Boolean = false,
      print_name: Boolean = false
  ): XML.Body = {

    def text_range_to_line(range: Text.Range): Line.Range = {
      val document = Line.Document(snapshot.node.source)
      document.range(range)
    }

    val location = when(
      print_location,
      text(s"At ${text_range_to_line(lint_result.range).start.print}:\n")
    )

    val message = text(s" ${lint_number + 1}. ${lint_result.message}")
    val lint_name =
      when(print_name, text(s" [lint name: ${lint_result.lint_name}]"))

    val edit = lint_result.edit match {
      case Some(edit) => text(" Consider: ") ::: edit_markup(edit)
      case None       => Nil
    }

    block(location ::: message ::: edit ::: lint_name)
  }

  /* xml helpers */

  private def when(b: Boolean, x: XML.Body): XML.Body = if (b) x else Nil

  private def edit_markup(edit: Linter.Edit): XML.Body = XML.Elem(
    Markup(
      Markup.LINTER,
      Position.Range(edit.range) ::: Markup.Content(edit.replacement)
    ),
    text(edit.message)
  ) :: Nil

  private def text(content: String): XML.Body = XML.Text(content) :: Nil

  private def block(inner: XML.Body): XML.Body =
    XML.elem(Markup.Block.name, inner) :: Nil

}
