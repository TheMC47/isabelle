/*  Title:      Pure/Thy/presentation.scala
    Author:     Makarius

HTML presentation of PIDE document content.
*/

package isabelle


import scala.collection.immutable.SortedMap


object Presentation
{
  /** HTML documents **/

  /* HTML context */

  val fonts_path = Path.explode("fonts")

  sealed case class HTML_Document(title: String, content: String)

  def html_context(fonts_url: String => String = HTML.fonts_url()): HTML_Context =
    new HTML_Context(fonts_url)

  final class HTML_Context private[Presentation](fonts_url: String => String)
  {
    def init_fonts(dir: Path): Unit =
    {
      val fonts_dir = Isabelle_System.make_directory(dir + fonts_path)
      for (entry <- Isabelle_Fonts.fonts(hidden = true))
        Isabelle_System.copy_file(entry.path, fonts_dir)
    }

    def head(title: String, rest: XML.Body = Nil): XML.Tree =
      HTML.div("head", HTML.chapter(title) :: rest)

    def source(body: XML.Body): XML.Tree = HTML.pre("source", body)

    def contents(heading: String, items: List[XML.Body], css_class: String = "contents")
      : List[XML.Elem] =
    {
      if (items.isEmpty) Nil
      else List(HTML.div(css_class, List(HTML.section(heading), HTML.itemize(items))))
    }

    def output_document(title: String, body: XML.Body): String =
      HTML.output_document(
        List(
          HTML.style(HTML.fonts_css(fonts_url) + "\n\n" + File.read(HTML.isabelle_css)),
          HTML.title(title)),
        List(HTML.source(body)), css = "", structural = false)

    def html_document(title: String, body: XML.Body): HTML_Document =
      HTML_Document(title, output_document(title, body))
  }


  /* presentation elements */

  sealed case class Elements(
    html: Markup.Elements = Markup.Elements.empty,
    entity: Markup.Elements = Markup.Elements.empty,
    language: Markup.Elements = Markup.Elements.empty)

  val elements1: Elements =
    Elements(
      html =
        Rendering.foreground_elements ++ Rendering.text_color_elements +
        Markup.NUMERAL + Markup.COMMENT + Markup.ENTITY + Markup.LANGUAGE,
      entity = Markup.Elements(Markup.THEORY))

  val elements2: Elements =
    Elements(
      html = elements1.html ++ Rendering.markdown_elements,
      language = Markup.Elements(Markup.Language.DOCUMENT))


  /* HTML output */

  private val div_elements =
    Set(HTML.div.name, HTML.pre.name, HTML.par.name, HTML.list.name, HTML.enum.name,
      HTML.descr.name)

  def make_html(
    elements: Elements,
    entity_link: (Properties.T, XML.Body) => Option[XML.Tree],
    xml: XML.Body): XML.Body =
  {
    def html_div(html: XML.Body): Boolean =
      html exists {
        case XML.Elem(markup, body) => div_elements.contains(markup.name) || html_div(body)
        case XML.Text(_) => false
      }

    def html_class(c: String, html: XML.Body): XML.Body =
      if (c == "") html
      else if (html_div(html)) List(HTML.div(c, html))
      else List(HTML.span(c, html))

    def html_body(xml_body: XML.Body): XML.Body =
      xml_body flatMap {
        case XML.Wrapped_Elem(markup, _, body) => html_body(List(XML.Elem(markup, body)))
        case XML.Elem(Markup(Markup.ENTITY, props @ Markup.Kind(kind)), body) =>
          val body1 = html_body(body)
          if (elements.entity(kind)) {
            entity_link(props, body1) match {
              case Some(link) => List(link)
              case None => body1
            }
          }
          else body1
        case XML.Elem(Markup(Markup.LANGUAGE, Markup.Name(name)), body) =>
          html_class(if (elements.language(name)) name else "", html_body(body))
        case XML.Elem(Markup(Markup.MARKDOWN_PARAGRAPH, _), body) =>
          List(HTML.par(html_body(body)))
        case XML.Elem(Markup(Markup.MARKDOWN_ITEM, _), body) =>
          List(HTML.item(html_body(body)))
        case XML.Elem(Markup(Markup.Markdown_Bullet.name, _), _) => Nil
        case XML.Elem(Markup.Markdown_List(kind), body) =>
          if (kind == Markup.ENUMERATE) List(HTML.enum(html_body(body)))
          else List(HTML.list(html_body(body)))
        case XML.Elem(markup, body) =>
          val name = markup.name
          val html =
            markup.properties match {
              case Markup.Kind(kind) if kind == Markup.COMMAND || kind == Markup.KEYWORD =>
                html_class(kind, html_body(body))
              case _ =>
                html_body(body)
            }
          Rendering.foreground.get(name) orElse Rendering.text_color.get(name) match {
            case Some(c) => html_class(c.toString, html)
            case None => html_class(name, html)
          }
        case XML.Text(text) =>
          HTML.text(Symbol.decode(text))
      }

    html_body(xml)
  }


  /* PIDE HTML document */

  def html_document(
    resources: Resources,
    snapshot: Document.Snapshot,
    html_context: HTML_Context,
    elements: Elements,
    plain_text: Boolean = false): HTML_Document =
  {
    require(!snapshot.is_outdated, "document snapshot outdated")

    val name = snapshot.node_name
    if (plain_text) {
      val title = "File " + Symbol.cartouche_decoded(name.path.file_name)
      val body = HTML.text(snapshot.node.source)
      html_context.html_document(title, body)
    }
    else {
      resources.html_document(snapshot) getOrElse {
        val title =
          if (name.is_theory) "Theory " + quote(name.theory_base_name)
          else "File " + Symbol.cartouche_decoded(name.path.file_name)
        val body = make_html(elements, (_, _) => None, snapshot.xml_markup(elements = elements.html))
        html_context.html_document(title, body)
      }
    }
  }



  /** HTML presentation **/

  /* presentation context */

  object Context
  {
    val none: Context = new Context { def enabled: Boolean = false }
    val standard: Context = new Context { def enabled: Boolean = true }

    def dir(path: Path): Context =
      new Context {
        def enabled: Boolean = true
        override def dir(store: Sessions.Store): Path = path
      }

    def make(s: String): Context =
      if (s == ":") standard else dir(Path.explode(s))
  }

  abstract class Context private
  {
    def enabled: Boolean
    def enabled(info: Sessions.Info): Boolean = enabled || info.browser_info
    def dir(store: Sessions.Store): Path = store.presentation_dir
    def dir(store: Sessions.Store, info: Sessions.Info): Path =
      dir(store) + Path.explode(info.chapter_session)
  }


  /* maintain chapter index */

  private val sessions_path = Path.basic(".sessions")

  private def read_sessions(dir: Path): List[(String, String)] =
  {
    val path = dir + sessions_path
    if (path.is_file) {
      import XML.Decode._
      list(pair(string, string))(Symbol.decode_yxml(File.read(path)))
    }
    else Nil
  }

  private def write_sessions(dir: Path, sessions: List[(String, String)]): Unit =
  {
    import XML.Encode._
    File.write(dir + sessions_path, YXML.string_of_body(list(pair(string, string))(sessions)))
  }

  def update_chapter_index(
    browser_info: Path, chapter: String, new_sessions: List[(String, String)]): Unit =
  {
    val dir = Isabelle_System.make_directory(browser_info + Path.basic(chapter))

    val sessions0 =
      try { read_sessions(dir) }
      catch { case _: XML.Error => Nil }

    val sessions = (SortedMap.empty[String, String] ++ sessions0 ++ new_sessions).toList
    write_sessions(dir, sessions)

    val title = "Isabelle/" + chapter + " sessions"
    HTML.write_document(dir, "index.html",
      List(HTML.title(title + Isabelle_System.isabelle_heading())),
      HTML.chapter(title) ::
       (if (sessions.isEmpty) Nil
        else
          List(HTML.div("sessions",
            List(HTML.description(
              sessions.map({ case (name, description) =>
                val descr = Symbol.trim_blank_lines(description)
                (List(HTML.link(name + "/index.html", HTML.text(name))),
                  if (descr == "") Nil
                  else HTML.break ::: List(HTML.pre(HTML.text(descr)))) })))))))
  }

  def make_global_index(browser_info: Path): Unit =
  {
    if (!(browser_info + Path.explode("index.html")).is_file) {
      Isabelle_System.make_directory(browser_info)
      Isabelle_System.copy_file(Path.explode("~~/lib/logo/isabelle.gif"),
        browser_info + Path.explode("isabelle.gif"))
      val title = "The " + XML.text(Isabelle_System.isabelle_name()) + " Library"
      File.write(browser_info + Path.explode("index.html"),
        HTML.header +
"""
<head>
  """ + HTML.head_meta + """
  <title>""" + title + """</title>
</head>

<body text="#000000" bgcolor="#FFFFFF" link="#0000FF" vlink="#000099" alink="#404040">
  <center>
    <table width="100%" border="0" cellspacing="10" cellpadding="0">
      <tr>
        <td width="20%" valign="middle" align="center"><a href="http://isabelle.in.tum.de/"><img align="bottom" src="isabelle.gif" width="100" height="86" alt="[Isabelle]" border="0" /></a></td>

        <td width="80%" valign="middle" align="center">
          <table width="90%" border="0" cellspacing="0" cellpadding="20">
            <tr>
              <td valign="middle" align="center" bgcolor="#AACCCC"><font face="Helvetica,Arial" size="+2">""" + title + """</font></td>
            </tr>
          </table>
        </td>
      </tr>
    </table>
  </center>
  <hr />
  <ul>
    <li>Higher-Order Logic</li>

    <li style="list-style: none">
      <ul>
        <li><a href="HOL/index.html">HOL (Higher-Order Logic)</a>
        is a version of classical higher-order logic resembling
        that of the <a href="http://www.cl.cam.ac.uk/Research/HVG/HOL/">HOL System</a>.
        </li>
      </ul>
    </li>
  </ul>

  <ul>
    <li>First-Order Logic</li>

    <li style="list-style: none">
      <ul>
        <li><a href="FOL/index.html">FOL (Many-sorted First-Order Logic)</a>
        provides basic classical and intuitionistic first-order logic. It is
        polymorphic.
        </li>

        <li><a href="ZF/index.html">ZF (Set Theory)</a>
        offers a formulation of Zermelo-Fraenkel set theory on top of FOL.
        </li>

        <li><a href="CCL/index.html">CCL (Classical Computational Logic)</a></li>

        <li><a href="LCF/index.html">LCF (Logic of Computable Functions)</a></li>

        <li><a href="FOLP/index.html">FOLP (FOL with Proof Terms)</a></li>
      </ul>
    </li>
  </ul>

  <ul>
    <li>Miscellaneous</li>

    <li style="list-style: none">
      <ul>
        <li><a href="Sequents/index.html">Sequents (first-order, modal and linear logics)</a></li>

        <li><a href="CTT/index.html">CTT (Constructive Type Theory)</a>
        is an extensional version of Martin-L&ouml;f's Type Theory.</li>

        <li><a href="Cube/index.html">Cube (The Lambda Cube)</a></li>

        <li><a href="Pure/index.html">The Pure logical framework</a></li>

        <li><a href="Doc/index.html">Sources of Documentation</a></li>
      </ul>
    </li>
  </ul>
</body>
""" + HTML.footer)
    }
  }


  /* present session */

  val session_graph_path = Path.explode("session_graph.pdf")
  val readme_path = Path.explode("README.html")
  val files_path = Path.explode("files")

  def html_name(name: Document.Node.Name): String = name.theory_base_name + ".html"

  def theory_link(deps: Sessions.Deps, session0: String,
    name: Document.Node.Name, body: XML.Body): Option[XML.Tree] =
  {
    val session1 = deps(session0).theory_qualifier(name)
    val info0 = deps.sessions_structure.get(session0)
    val info1 = deps.sessions_structure.get(session1)
    if (info0.isDefined && info1.isDefined) {
      Some(HTML.link(info0.get.relative_path(info1.get) + html_name(name), body))
    }
    else None
  }

  def session_html(
    resources: Resources,
    session: String,
    deps: Sessions.Deps,
    db_context: Sessions.Database_Context,
    progress: Progress = new Progress,
    verbose: Boolean = false,
    html_context: HTML_Context,
    elements: Elements,
    presentation: Context): Unit =
  {
    val info = deps.sessions_structure(session)
    val options = info.options
    val base = deps(session)

    val session_dir = presentation.dir(db_context.store, info)

    html_context.init_fonts(session_dir)

    Bytes.write(session_dir + session_graph_path,
      graphview.Graph_File.make_pdf(options, base.session_graph_display))

    val documents =
      for {
        doc <- info.document_variants
        document <- db_context.input_database(session)(Document_Build.read_document(_, _, doc.name))
      } yield {
        if (verbose) progress.echo("Presenting document " + session + "/" + doc.name)
        Bytes.write(session_dir + doc.path.pdf, document.pdf)
        doc
      }

    val view_links =
    {
      val deps_link =
        HTML.link(session_graph_path, HTML.text("theory dependencies"))

      val readme_links =
        if ((info.dir + readme_path).is_file) {
          Isabelle_System.copy_file(info.dir + readme_path, session_dir + readme_path)
          List(HTML.link(readme_path, HTML.text("README")))
        }
        else Nil

      val document_links =
        documents.map(doc => HTML.link(doc.path.pdf, HTML.text(doc.name)))

      Library.separate(HTML.break ::: HTML.nl,
        (deps_link :: readme_links ::: document_links).
          map(link => HTML.text("View ") ::: List(link))).flatten
    }

    val theories: List[XML.Body] =
    {
      var seen_files = List.empty[(Path, String, Document.Node.Name)]

      def entity_link(props: Properties.T, body: XML.Body): Option[XML.Tree] =
        (props, props, props) match {
          case (Markup.Kind(Markup.THEORY), Markup.Name(theory), Position.Def_File(thy_file)) =>
            val node_name = resources.file_node(Path.explode(thy_file), theory = theory)
            theory_link(deps, session, node_name, body)
          case _ => None
        }

      sealed case class Theory(
        name: Document.Node.Name,
        command: Command,
        files_html: List[(Path, XML.Tree)],
        html: XML.Tree)

      def read_theory(name: Document.Node.Name): Option[Theory] =
      {
        progress.expose_interrupt()
        if (verbose) progress.echo("Presenting theory " + name)

        for (command <- Build_Job.read_theory(db_context, resources, session, name.theory))
        yield {
          val snapshot = Document.State.init.snippet(command)

          val files_html =
            for {
              (src_path, xml) <- snapshot.xml_markup_blobs(elements = elements.html)
              if xml.nonEmpty
            }
            yield {
              progress.expose_interrupt()
              if (verbose) progress.echo("Presenting file " + src_path)

              (src_path, html_context.source(make_html(elements, entity_link, xml)))
            }

          val html =
            html_context.source(
              make_html(elements, entity_link, snapshot.xml_markup(elements = elements.html)))

          Theory(name, command, files_html, html)
        }
      }

      for (thy <- Par_List.map(read_theory, base.session_theories).flatten)
      yield {
        val files =
          for { (src_path, file_html) <- thy.files_html }
          yield {
            val file_name = (files_path + src_path.squash.html).implode

            seen_files.find(p => p._1 == src_path || p._2 == file_name) match {
              case None => seen_files ::= (src_path, file_name, thy.name)
              case Some((_, _, thy_name1)) =>
                error("Incoherent use of file name " + src_path + " as " + quote(file_name) +
                  " in theory " + thy_name1 + " vs. " + thy.name)
            }

            val file_path = session_dir + Path.explode(file_name)
            html_context.init_fonts(file_path.dir)

            val file_title = "File " + Symbol.cartouche_decoded(src_path.implode_short)
            HTML.write_document(file_path.dir, file_path.file_name,
              List(HTML.title(file_title)), List(html_context.head(file_title), file_html))

            List(HTML.link(file_name, HTML.text(file_title)))
          }

        val thy_title = "Theory " + thy.name.theory_base_name

        HTML.write_document(session_dir, html_name(thy.name),
          List(HTML.title(thy_title)), List(html_context.head(thy_title), thy.html))

        List(HTML.link(html_name(thy.name),
          HTML.text(thy.name.theory_base_name) :::
            (if (files.isEmpty) Nil else List(HTML.itemize(files)))))
      }
    }

    val title = "Session " + session
    HTML.write_document(session_dir, "index.html",
      List(HTML.title(title + Isabelle_System.isabelle_heading())),
      html_context.head(title, List(HTML.par(view_links))) ::
        html_context.contents("Theories", theories))
  }
}
