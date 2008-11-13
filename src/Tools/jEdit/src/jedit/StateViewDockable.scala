package isabelle.jedit


import java.awt.BorderLayout
import javax.swing.{ JButton, JPanel, JScrollPane }

import isabelle.IsabelleSystem.getenv

import org.w3c.dom.Document

import org.xhtmlrenderer.simple.XHTMLPanel
import org.xhtmlrenderer.context.AWTFontResolver

import org.gjt.sp.jedit.View

//Copy-Paste-support
import org.w3c.dom.ranges.Range
import org.w3c.dom.DocumentFragment
import org.xhtmlrenderer.swing.SelectionHighlighter

class StateViewDockable(view : View, position : String) extends JPanel {
  {
    val panel = new XHTMLPanel(new UserAgent())
    setLayout(new BorderLayout)

    //Copy-paste-support
    val clipboard = java.awt.Toolkit.getDefaultToolkit().getSystemClipboard;
    val sel_highlighter = new SelectionHighlighter

    val copyaction = new SelectionHighlighter.CopyAction {
      override def actionPerformed(e: java.awt.event.ActionEvent) {
        val selected_string = sel_highlighter.getSelectionRange.toString
        val encoded = VFS.converter.encode (selected_string)
        System.err.println ("-- copy --")
        System.err.println (selected_string)
        System.err.println (encoded)
        val transferable = new java.awt.datatransfer.StringSelection(encoded)
        clipboard.setContents(transferable, null)
      }
    }
    copyaction.install(sel_highlighter)
    sel_highlighter.install(panel)
    add(new JButton(copyaction), BorderLayout.SOUTH)

    add(new JScrollPane(panel), BorderLayout.CENTER)
    
    val fontResolver =
      panel.getSharedContext.getFontResolver.asInstanceOf[AWTFontResolver]
    if (Plugin.plugin.viewFont != null)
      fontResolver.setFontMapping("Isabelle", Plugin.plugin.viewFont)

    Plugin.plugin.viewFontChanged.add(font => {
      if (Plugin.plugin.viewFont != null)
        fontResolver.setFontMapping("Isabelle", Plugin.plugin.viewFont)
      
      panel.relayout()
    })
    
    Plugin.plugin.stateUpdate.add(state => {
      if (state == null)
        panel.setDocument(null : Document)
      else
        panel.setDocument(state.document, UserAgent.baseURL)
    })
  }
}
