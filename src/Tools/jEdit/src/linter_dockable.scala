package isabelle.jedit

import isabelle._
import isabelle.jedit_base.Dockable

import linter._

import scala.swing.{Button, CheckBox}
import scala.swing.event.ButtonClicked

import java.awt.BorderLayout
import java.awt.event.{ComponentEvent, ComponentAdapter}

import org.gjt.sp.jedit.View

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

class PIDE_Linter_Variable[A](reporter: Reporter[A])
    extends Linter_Variable[A](reporter, true) {

  private def refresh_lint(): Unit = {
    for {
      snapshot <- PIDE.maybe_snapshot()
      linter <- get
      if !snapshot.is_outdated
    } linter.do_lint(snapshot)

  }

  private val main =
    Session.Consumer[Any](getClass.getName) { case _ =>
      GUI_Thread.later {
        Isabelle_Thread.fork(name = "linter") {
          refresh_lint()
          // FIXME maybe a separate event for the linter?
          PIDE.session.caret_focus.post(Session.Caret_Focus)
        }
      }
    }

  def setup_handlers(): Unit =
    if (get.isEmpty) uninstall_handlers() else install_handlers()

  private def install_handlers(): Unit = {
    PIDE.session.global_options += main
    PIDE.session.commands_changed += main
  }
  private def uninstall_handlers(): Unit = {
    PIDE.session.global_options -= main
    PIDE.session.commands_changed -= main
  }

}

class Linter_Dockable(view: View, position: String)
    extends Dockable(view, position) {

  private var current_output: List[XML.Tree] = Nil

  val pretty_text_area = new Pretty_Text_Area(view)
  set_content(pretty_text_area)

  val separator = XML_Lint_Reporter.text("----------------")
  val disabled = XML_Lint_Reporter.text("The linter plugin is disabled.")

  override def detach_operation: Option[() => Unit] =
    pretty_text_area.detach_operation

  def handle_lint(do_lint: Boolean): Unit = {

    GUI_Thread.require {}

    for {
      snapshot <- PIDE.maybe_snapshot(view)
      if !snapshot.is_outdated && do_lint
    } {
      val new_output = PIDE.plugin.linter.get match {
        case None => disabled
        case Some(linter) =>
          val current_command =
            PIDE.editor.current_command(view, snapshot) match {
              case None          => Command.empty
              case Some(command) => command
            }

          val command_lints =
            linter.report_for_command(snapshot, current_command.id)

          lazy val snapshot_lints =
            linter.report_for_snapshot(snapshot)

          val all_lints =
            if (lint_all && !snapshot_lints.isEmpty)
              separator ::: snapshot_lints
            else Nil
          command_lints ::: all_lints
      }

      if (current_output != new_output) {
        pretty_text_area.update(
          snapshot,
          Command.Results.empty,
          Pretty.separate(new_output)
        )
        current_output = new_output
      }
    }
  }

  /* controls */

  private def lint_all: Boolean = PIDE.options.bool("lint_all")
  private def lint_all_=(b: Boolean): Unit = {
    if (lint_all != b) {
      PIDE.options.bool("lint_all") = b
      PIDE.editor.flush_edits(hidden = true)
      PIDE.editor.flush()
    }
  }

  private val lint_all_button = new CheckBox("Lint all") {
    tooltip = "Display lints of the whole document"
    reactions += { case ButtonClicked(_) =>
      lint_all = selected; handle_lint(true)
    }
    selected = lint_all
  }

  private var auto_lint: Boolean = true

  private val auto_lint_button = new CheckBox("Auto lint") {
    tooltip = "Indicate automatic lint following cursor movement"
    reactions += { case ButtonClicked(_) => auto_lint = this.selected }
    selected = auto_lint
  }

  private val lint_button = new Button("Lint") {
    tooltip = "Lint and update display"
    reactions += { case ButtonClicked(_) => handle_lint(true) }
  }

  private def linter: Boolean = PIDE.options.bool("linter")
  private def linter_=(b: Boolean): Unit = {
    if (linter != b) {
      PIDE.options.bool("linter") = b
      PIDE.plugin.linter.update(PIDE.options.value)
      PIDE.editor.flush_edits(hidden = true)
      PIDE.editor.flush()
    }
  }

  private val linter_button = new CheckBox("Linter") {
    tooltip = "State of the linter"
    reactions += { case ButtonClicked(_) =>
      linter = selected; handle_lint(true)
    }
    selected = linter
  }

  private val controls =
    Wrap_Panel(
      List(linter_button, auto_lint_button, lint_all_button, lint_button)
    )

  add(controls.peer, BorderLayout.NORTH)

  /* main */

  private val main =
    Session.Consumer[Any](getClass.getName) {
      case _: Session.Global_Options =>
        GUI_Thread.later { handle_lint(auto_lint) }

      case _: Session.Commands_Changed =>
        GUI_Thread.later { handle_lint(auto_lint) }

      case Session.Caret_Focus => GUI_Thread.later { handle_lint(auto_lint) }
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
