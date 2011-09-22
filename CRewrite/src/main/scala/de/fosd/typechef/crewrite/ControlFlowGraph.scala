package de.fosd.typechef.crewrite

import org.kiama.==>
import org.kiama.attribution.Attributable
import org.kiama.attribution.DynamicAttribution.{attr, childAttr}
import de.fosd.typechef.parser.c._
import de.fosd.typechef.conditional._
import de.fosd.typechef.featureexpr._
import java.io.PrintWriter

abstract sealed class CFCompleteness
case class CFComplete(s: List[AST]) extends CFCompleteness
case class CFIncomplete(s: List[AST]) extends CFCompleteness

// http://code.google.com/p/kiama/source/browse/src/org/kiama/example/dataflow/Dataflow.scala
trait ControlFlow {
  def succ(a: Attributable): List[AST]
}

trait ControlFlowImpl extends ControlFlow with ASTNavigation with ConditionalNavigation {

  private implicit def optList2ASTList(l: List[Opt[AST]]) = l.map(_.entry)
  private implicit def opt2AST(s: Opt[AST]) = s.entry

  implicit def list2TChoice(l: List[AST]): TConditional[AST] = {
    if (l.size == 1) TOne(l.head)
    else if (l.size == 2) TChoice(featureExpr(l.head), TOne(l.head), TOne(l.tail.head))
    else TChoice(featureExpr(l.head), TOne(l.head), list2TChoice(l.tail))
  }

  // handling of successor determination of a single statement
  def succ(a: Attributable): List[AST] = {
    a match {
      case o: Opt[AST] => succ(o.entry)
      case t@ForStatement(init, break, inc, b) => {
        if (init.isDefined) List(init.get)
        else if (break.isDefined) List(break.get)
        else simpleOrCompoundStatement(t, b)
      }
      case WhileStatement(e, _) => List(e)
      case t@DoStatement(_, b) => simpleOrCompoundStatement(t, b)
      case t@IfStatement(c, _, _, _) => List(c)
      case t@ElifStatement(c, _) => List(c)
      case SwitchStatement(c, _) => List(c)
      case ReturnStatement(_) => List()
      case w@CompoundStatement(l) => getSuccSameLevel(w) ++ getSuccNestedLevel(l)
      case w@BreakStatement() => {
        val f = followUp(w)
        if (f.isDefined) getSuccSameLevel(f.get.head) else List()
      }
      case w@ContinueStatement() => {
        val f = followUp(w)
        if (f.isDefined) f.get.head match {
          case t@ForStatement(_, break, inc, b) => {
            if (inc.isDefined) List(inc.get)
            else if (break.isDefined) List(break.get)
            else simpleOrCompoundStatement(t, b)
          }
          case WhileStatement(c, _) => List(c)
          case DoStatement(c, _) => List(c)
          case _ => List()
        } else List()
      }
      case w@GotoStatement(Id(l)) => {
        val f = findPriorFuncDefinition(w)
        if (f == null) List()
        else labelLookup(f, l)
      }
      case s: Statement => getSuccSameLevel(s)
      case t => following(t)
    }
  }
  private val findPriorFuncDefinition: AST ==> FunctionDef = attr {
    case f: FunctionDef => f
    case a: Attributable if (!a.isRoot) => findPriorFuncDefinition(parentAST(a))
    case _ => null
  }

  private def labelLookup(a: AST, l: String): List[AST] = {
    def iterateChildren(a: AST): List[AST] = {
      a.children.asInstanceOf[Iterator[Attributable]].map(
        x => x match {
          case e: AST => labelLookup(e, l)
          case e: Opt[AST] => labelLookup(childAST(e), l)
        }).foldLeft(List[AST]())(_ ++ _)
    }
    a match {
      case e @ LabelStatement(Id(n), _) if (n == l) => List(e) ++ iterateChildren(e)
      case e : AST => iterateChildren(e)
    }
  }

  private def simpleOrCompoundStatement(p: Statement, c: Conditional[_]) = {
    c.asInstanceOf[One[_]].value match {
      case CompoundStatement(l) => if (l.isEmpty) List(p) else getSuccNestedLevel(l)
      case s: Statement => List(s)
    }
  }

  // handling of successor determination of nested structures, such as for, while, ... and next element in a list
  // of statements
  private def following(a: Attributable): List[AST] = {
    parentAST(a.asInstanceOf[AST]) match {
      case t@ForStatement(Some(e), c, _, b) if e.eq(a) => if (c.isDefined) List(c.get) else simpleOrCompoundStatement(t, b)
      case t@ForStatement(_, Some(e), _, b) if e.eq(a) => getSuccSameLevel(t) ++ simpleOrCompoundStatement (t, b)
      case t@ForStatement(_, c, Some(e), b) if e.eq(a) => if (c.isDefined) List(c.get) else simpleOrCompoundStatement(t, b)
      case t@ForStatement(_, c, i, e) if e.eq(a)=> {
        if (i.isDefined) List(i.get)
        else if (c.isDefined) List(c.get)
        else simpleOrCompoundStatement(t, e)
      }
      case t@WhileStatement(e, b) if e.eq(a) => simpleOrCompoundStatement(t, b) ++ getSuccSameLevel(t)
      case t@DoStatement(e, b) if e.eq(a) => simpleOrCompoundStatement(t, b) ++ getSuccSameLevel(t)
      case t@IfStatement(e, tb, elif, el) if e.eq(a) => {
        var res = simpleOrCompoundStatement(t, tb)
        if (! elif.isEmpty) res = res ++ getSuccNestedLevel(elif)
        if (el.isDefined) res = res ++ simpleOrCompoundStatement(t, el.get)
        res
      }
      case t@ElifStatement(e, One(CompoundStatement(l))) if e.eq(a) => getSuccNestedLevel(l) ++ getSuccSameLevel(t)
      case _ => List()
    }
  }

  // method to catch surrounding while, for, ... statement, which is the follow item of a last element in it's list
  private def followUp(n: Attributable): Option[List[AST]] = {
    n.parent[Attributable] match {
      case c: CompoundStatement => followUp(c)
      case w @ WhileStatement(e, _) => Some(List(e))
      case w : ForStatement => Some(List(w))
      case w @ DoStatement(e, One(CompoundStatement(l))) => Some(List(e))
      case w @ IfStatement(_, _, _, _) => Some(getSuccSameLevel(w))
      case w @ ElifStatement(_, _) => followUp(w)
      case o: Opt[_] => followUp(o)
      case c: Conditional[_] => followUp(c)
      case s: Statement => followUp(s)
      case _ => None
    }
  }

  // we have to check possible successor nodes in at max three steps:
  // 1. get direct successors with same annotation; if yes stop; if not goto 2.
  // 2. get all annotated elements at the same level and check whether we find a definite set of successor nodes
  //    if yes stop; if not goto 3.
  // 3. get the parent of our node and determine successor nodes of it
  private def getSuccSameLevel(s: AST) = {
    val sandf = getFeatureGroupedASTElems(s)
    val sos = getNextEqualAnnotatedSucc(s, sandf)
    sos match {
      // 1.
      case Some(x) => List(x)
      case None => {
        val succel = getSuccFromList(featureExpr(s), sandf.drop(1))
        succel match {
          case CFComplete(r) => r // 2.
          case CFIncomplete(r) => r ++ followUp(s).getOrElse(List()) // 3.
        }
      }
    }
  }

  private def getSuccNestedLevel(l: List[AST]) = {
    if (l.isEmpty) List()
    else {
      val wsandf = determineTypeOfGroupedOptLists(groupOptListsImplication(groupOptBlocksEquivalence(l)).reverse).reverse
      val succel = getSuccFromList(featureExpr(parentOpt(l.head)), wsandf)

      succel match {
        case CFComplete(r) => r
        case CFIncomplete(r) => r ++ followUp(l.head).getOrElse(List())
      }
    }
  }

  // pack similar elements into sublists
  private def pack[T](f: (T, T) => Boolean)(l: List[T]): List[List[T]] = {
    if (l.isEmpty) List()
    else (l.head::l.tail.takeWhile(f(l.head, _)))::pack(f)(l.tail.dropWhile(f(l.head, _)))
  }

  // group consecutive Opts in a list and return a list of list containing consecutive (feature equivalent) opts
  // e.g.:
  // List(Opt(true, Id1), Opt(fa, Id2), Opt(fa, Id3)) => List(List(Opt(true, Id1)), List(Opt(fa, Id2), Opt(Id3)))
  private def groupOptBlocksEquivalence(l: List[AST]) = {
    pack[AST](parentOpt(_).feature equivalentTo parentOpt(_).feature)(l)
  }

  // group List[Opt[_]] according to implication
  // later one should imply the not of previous ones; therefore using l.reverse
  private def groupOptListsImplication(l: List[List[AST]]) = {
    def checkImplication(a: AST, b: AST) = {
      val as = featureExprSet(a)
      val bs = featureExprSet(b)
      val cs = as.intersect(bs)
      as.--(cs).foldLeft(FeatureExpr.base)(_ and _).implies(bs.--(cs).foldLeft(FeatureExpr.base)(_ and _).not).isTautology()
    }
    pack[List[AST]]({ (x,y) => checkImplication(x.head, y.head)})(l.reverse).reverse
  }

  // get type of List[List[AST]:
  // 0 -> only true values
  // 1 -> #if-(#elif)* block
  // 2 -> #if-(#elif)*-#else block
  private def determineTypeOfGroupedOptLists(l: List[List[List[AST]]]): List[(Int, List[List[AST]])] = {
    l match {
      case (h::t) => {
        val f = h.map({ x => featureExpr(x.head)})
        if (f.foldLeft(FeatureExpr.base)(_ and _).isTautology()) (0, h)::determineTypeOfGroupedOptLists(t)
        else if (f.map(_.not).foldLeft(FeatureExpr.base)(_ and _).isContradiction()) (2, h.reverse)::determineTypeOfGroupedOptLists(t)
             else (1, h)::determineTypeOfGroupedOptLists(t)
      }
      case Nil => List()
    }
  }

  // returns a list of previous and next AST elems grouped according to feature expressions
  private def getFeatureGroupedASTElems(s: AST) = {
    val l = prevASTElems(s) ++ nextASTElems(s).drop(1)
    val d = determineTypeOfGroupedOptLists(groupOptListsImplication(groupOptBlocksEquivalence(l)))
    getSuccTailList(s, d)
  }

  // get all succ nodes of o
  private def getNextEqualAnnotatedSucc(o: AST, l: List[(Int, List[List[AST]])]): Option[AST] = {
    if (l.isEmpty) return None
    var el = l.head

    // take tuple with o and examine it
    // _.map(_.eq(o)).max compares object identity and not structural identity as list.contains does
    val il = el._2.filter(_.map(_.eq(o)).max)
    val jl = il.head.span(_.ne(o))._2.drop(1)
    if (! jl.isEmpty) Some(jl.head)
    else None
  }

  // get list with o and all following lists
  private def getSuccTailList(o: AST, l: List[(Int, List[List[AST]])]): List[(Int, List[List[AST]])] = {
    // get the list with o and all following lists
    // iterate each sublist of the incoming tuples (Int, List[List[Opt[_]]] combine equality check
    // with foldLeft and drop tuples in which o does not occur
    l.dropWhile(_._2.map(_.map(_.eq(o)).foldLeft(false)(_ || _)).foldLeft(false)(_ || _).unary_!)
  }

  // get all succ nodes of an unknown input node; useful for cases in which successor nodes occur
  // in a different block
  private def getSuccFromList(c: FeatureExpr, l: List[(Int, List[List[AST]])]): CFCompleteness = {
    var r = List[AST]()
    for (e <- l) {
      e match {
        case (0, opts) => r = r ++ List(opts.head.head)
        case (_, opts) => r = r ++ opts.map({ x=> List(x.head)}).foldLeft(List[AST]())(_ ++ _)
      }

      if (e._1 == 2 || e._1 == 0) return CFComplete(r)
      if (featureExpr(e._2.head.head).equivalentTo(c) && e._1 == 1) return CFComplete(r)
    }
    CFIncomplete(r)
  }

  // determine recursively all succs
  def getAllSucc(i: AST) = {
    var r = List[(AST, List[AST])]()
    var s = List(i)
    var d = List[AST]()
    var c: AST = null

    while (! s.isEmpty) {
      c = s.head
      s = s.drop(1)

      if (d.filter(_.eq(c)).isEmpty) {
        r = (c, succ(c)) :: r
        s = s ++ r.head._2
        d = d ++ List(c)
      }
    }
    r
  }
}

trait IOUtilities {
  // http://stackoverflow.com/questions/4604237/how-to-write-to-a-file-in-scala
  import java.io.FileWriter
  def using[A <: {def close(): Unit}, B](param: A)(f: A => B): B =
    try { f(param) } finally { param.close() }

  def writeToFile(fileName:String, data:String) =
    using (new FileWriter(fileName)) {
      fileWriter => fileWriter.write(data)
    }

  def appendToFile(fileName:String, textData:String) =
    using (new FileWriter(fileName, true)){
      fileWriter => using (new PrintWriter(fileWriter)) {
        printWriter => printWriter.println(textData)
      }
    }
}

object DotGraph extends IOUtilities with ASTNavigation with FeatureExprLookup {
  import java.io.File

  private def getTmpFileName() = File.createTempFile("/tmp", ".dot")
  def map2file(m: List[(AST, List[AST])]) = {
    println(m)
    var dotstring = ""
    val fname = getTmpFileName()
    dotstring += "digraph \"" + fname.getName + "\" {" + "\n"
    dotstring += "node [shape=record];\n"
    for ((o, succs) <- m) {
      val op = esc(PrettyPrinter.print(o))
      dotstring += "\"" + System.identityHashCode(o) + "\" [label=\"{{" + op + "}|" + esc(featureExpr(o).toString()) + "}\"];\n"
      for (succ <- succs) dotstring += "\"" + System.identityHashCode(o) + "\" -> \"" + System.identityHashCode(succ) + "\"\n"
    }
    dotstring = dotstring + "}\n"
    println(dotstring)
    writeToFile(fname.getAbsolutePath, dotstring)
  }

  private def esc(i: String) = {
    i.replace("\n", "\\n").replace("{", "\\{").replace("}", "\\}").replace("<", "\\<").replace(">", "\\>")
  }

}

trait Variables {
  val uses: AST ==> Set[Id]
  val defines: AST ==> Set[Id]
}

trait VariablesImpl extends Variables with ASTNavigation {
  val uses: Attributable ==> Set[Id] =
    attr {
      case InitDeclaratorI(declarator, _, Some(i)) => uses(declarator)// ++ i->uses
      case AtomicNamedDeclarator(_, id, _) => Set(id)
      case NestedNamedDeclarator(_, nestedDecl, _) => uses(nestedDecl)
      case Initializer(_, expr) => uses(expr)
      case Id(name) => Set(Id(name))
      case Constant(_) => Set()
      case StringLit(_) => Set()
      case SimplePostfixSuffix(_) => Set()
      case PointerPostfixSuffix(kind, id) => Set(id)
      case FunctionCall(params) => Set() // TODO List[Opt[Expr]]
      case ArrayAccess(expr) => uses(expr)
      case PostfixExpr(p, s) => uses(p)// ++ s->uses
      case UnaryExpr(_, e) => uses(e)
      case SizeOfExprT(_) => Set()
      case SizeOfExprU(expr) => uses(expr)
      case CastExpr(_, expr) => uses(expr)
      case PointerDerefExpr(castExpr) => uses(castExpr)
      case PointerCreationExpr(castExpr) => uses(castExpr)
      case UnaryOpExpr(kind, castExpr) => uses(castExpr)
      case NAryExpr(e, others) => uses(e) // TODO others List[Opt[NarySubExpr]]
      case NArySubExpr(_, e) => uses(e)
      case ConditionalExpr(condition, Some(thenExpr), elseExpr) => uses(condition) //++ thenExpr->uses ++ elseExpr->uses
      case AssignExpr(target, _, _) => uses(target)
      case ExprList(_) => Set()
      case _ => Set()
    }


  val defines: Attributable ==> Set[Id] =
    attr {
      case CompoundStatement(innerStatements) => innerStatements.map(defines).foldLeft(Set[Id]())(_ ++ _)
      case DeclarationStatement(decl) => uses(decl)
      case Declaration(_, init) => init.map(defines).foldLeft(Set[Id]())(_ ++ _)
      case InitDeclaratorI(declarator, _, _) => uses(declarator)
      case AtomicNamedDeclarator(_, id, _) => Set(id)
      case WhileStatement(_, s) => uses(s)
      case DoStatement(_, s) => uses(s)
      case ForStatement(_, _, _, s) => uses(s)
      case CaseStatement(_, Some(s)) => uses(s)
      case DefaultStatement(Some(s)) => uses(s)
      case SwitchStatement(_, s) => uses(s)
      case IfStatement(_, thenBranch, elifs, elseBranch) => {
          var r = uses(thenBranch) ++ elifs.map(defines).foldLeft(Set[Id]())(_ ++ _)
          if (elseBranch.getOrElse("") != "")
            r ++ defines(elseBranch.get)
          else
            r
      }
      case ElifStatement(_, thenBranch) => defines(childAST(thenBranch))
      case o@Opt(_, _) => defines(childAST(o))
      case o@One(_) => defines(childAST(o))
      case c@Choice(_, thenBranch, elseBranch) => defines(thenBranch) ++ defines(elseBranch)
      case _ => Set()
    }
}


