package mainelib
import scala.util.Try
import scala.reflect.runtime.{universe => ru}


object CfgPrinter {

  def printAll(cfg: Any, title: String = "CONFIG", maxDepth: Int = 4): Unit = {
    println(s"============ $title ============")
    println(render(cfg, 0, maxDepth))
    println(s"============ END $title ============")
  }

  // ---- helpers ----
  private def isCaseClassLike(a: Any): Boolean =
    a != null && a.isInstanceOf[Product]       // 不再用 className.contains("$")

  private def isSimple(a: Any): Boolean = a match {
    case null | _: String | _: Char | _: Boolean |
         _: java.lang.Number | _: BigInt | _: BigDecimal => true
    case _ => false
  }

  // Scala 2.12：用反射拿 case class 主构造参数名
  private def caseClassFields(p: Product): Seq[(String, Any)] = {
    val mirror = ru.runtimeMirror(p.getClass.getClassLoader)
    val sym    = mirror.classSymbol(p.getClass)
    val tpe    = sym.toType
    val ctor   = tpe.decls.collectFirst { case m: ru.MethodSymbol if m.isPrimaryConstructor => m }.get
    val names  = ctor.paramLists.flatten.map(_.name.toString)
    names.zip(p.productIterator.toSeq)
  }

  private def noArgDefs(obj: Any): Seq[(String, Any)] = {
    if (obj == null) return Seq.empty
    val cls = obj.getClass
    val blacklist = Set(
      "hashCode","toString","equals","canEqual","productArity",
      "productElement","productElementName","productIterator","productPrefix","copy"
    )
    cls.getMethods.toSeq
      .filter(m =>
        m.getParameterCount == 0 &&
          m.getReturnType != classOf[Unit] &&
          m.getDeclaringClass == cls &&
          !m.getName.contains("$") &&
          !blacklist.contains(m.getName)
      )
      .flatMap(m => Try(m.invoke(obj)).toOption.map(v => m.getName -> v))
  }

  private def render(x: Any, depth: Int, maxDepth: Int): String = {
    val ind  = "  " * depth
    val ind2 = "  " * (depth + 1)
    if (x == null) return s"${ind}null"
    if (depth >= maxDepth || isSimple(x)) return s"$ind$x"

    x match {
      case p: Product if isCaseClassLike(p) =>
        val flds = caseClassFields(p)
        val defs = noArgDefs(p)
        val lines =
          (if (flds.nonEmpty) Seq(s"${ind}{ // fields") else Seq.empty) ++
            flds.map { case (n, v) => s"$ind2$n = ${renderInline(v, depth + 1, maxDepth)}" } ++
            (if (defs.nonEmpty) Seq(s"$ind  // derived defs") else Seq.empty) ++
            defs.map { case (n, v) => s"$ind2$n = ${renderInline(v, depth + 1, maxDepth)}" } ++
            (if (flds.nonEmpty || defs.nonEmpty) Seq(s"$ind}") else Seq(s"$ind{}"))
        lines.mkString("\n")

      case m: scala.collection.Map[_, _] =>
        val elems = m.toSeq.sortBy(_._1.toString)
          .map { case (k, v) => s"$ind2${k.toString} -> ${renderInline(v, depth + 1, maxDepth)}" }
        (Seq(s"${ind}{") ++ elems ++ Seq(s"$ind}")).mkString("\n")

      case it: Iterable[_] =>
        val elems = it.zipWithIndex.map { case (v, i) => s"$ind2[$i] = ${renderInline(v, depth + 1, maxDepth)}" }.toSeq
        (Seq(s"${ind}[") ++ elems ++ Seq(s"$ind]")).mkString("\n")

      case opt: Option[_] =>
        opt match { case Some(v) => s"$ind Some(${renderInline(v, depth + 1, maxDepth)})"; case None => s"$ind None" }

      case other => s"$ind${other.toString}"
    }
  }

  private def renderInline(x: Any, depth: Int, maxDepth: Int): String = {
    if (x == null) return "null"
    if (isSimple(x)) return x.toString
    x match {
      case it: Iterable[_] if it.isEmpty => "[]"
      case m: scala.collection.Map[_, _] if m.isEmpty => "{}"
      case p: Product if isCaseClassLike(p) =>
        val flds = caseClassFields(p)
        val defs = noArgDefs(p)
        val kvs  = (flds ++ defs).map { case (n, v) => s"$n=${short(v)}" }
        val one  = kvs.mkString("{", ", ", "}")
        if (one.length <= 120) one else s"\n${render(p, depth, maxDepth)}"
      case _ =>
        val s = x.toString
        if (s.length <= 120) s else s"\n${render(x, depth, maxDepth)}"
    }
  }

  private def short(v: Any): String =
    if (v == null) "null"
    else if (isSimple(v)) v.toString
    else v match {
      case it: Iterable[_]               => s"Iterable(size=${it.size})"
      case m: scala.collection.Map[_, _] => s"Map(size=${m.size})"
      case p: Product if isCaseClassLike(p) => p.productPrefix
      case _ => v.getClass.getSimpleName
    }
}

