package de.fosd.typechef.crewrite

import org.kiama.==>
import org.kiama.attribution.Attributable
import org.kiama.attribution.DynamicAttribution.{attr, childAttr}
import de.fosd.typechef.parser.c._
import de.fosd.typechef.conditional._
import de.fosd.typechef.featureexpr._
import java.io.PrintWriter

abstract sealed class CFCompleteness
case class CFComplete(s: Set[AST]) extends CFCompleteness
case class CFIncomplete(s: Set[AST]) extends CFCompleteness

// http://code.google.com/p/kiama/source/browse/src/org/kiama/example/dataflow/Dataflow.scala
trait ControlFlow {
  val succ: Attributable ==> Set[AST]
}

trait ControlFlowImpl extends ControlFlow with ASTNavigation with ConditionalNavigation {

  val succ: Attributable ==> Set[AST] =
    attr {
      case o: Opt[AST] => succ(o.entry)
      case w@WhileStatement(_, One(CompoundStatement(l))) => {
        getSuccSameLevel(w) ++ getSuccNestedLevel(l.map(_.entry))
      }
      case w@ForStatement(_, _, _, One(CompoundStatement(l))) => {
        getSuccSameLevel(w) ++ getSuccNestedLevel(l.map(_.entry))
      }
      case w@DoStatement(_, One(CompoundStatement(l))) => {
        getSuccSameLevel(w) ++ getSuccNestedLevel(l.map(_.entry))
      }
      case w@CompoundStatement(l) => {
        getSuccSameLevel(w) ++ getSuccNestedLevel(l.map(_.entry))
      }
      case s: Statement => {
        getSuccSameLevel(s)
      }
      case _ => Set()
    }

  // method to catch surrounding while, for, ... statement, which is the follow item of a last element in it's list
  private def followUp(n: Attributable, fenv: Boolean = false): Option[Set[AST]] = {
    n.parent[Attributable] match {
      case c: CompoundStatement => followUp(c, true)
      case w: WhileStatement => Some(Set(w))
      case w: ForStatement => Some(Set(w))
      case w: DoStatement => Some(Set(w))
      case s: Statement => if (fenv) None else followUp(s, fenv)
      case o: Opt[_] => followUp(o, fenv)
      case c: Conditional[_] => followUp(c, fenv)
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
      case Some(x) => Set(x)
      case None => {
        val foll = sandf.drop(1)
        val succel = getSuccFromList(featureExpr(s), foll)
        succel match {
          case CFComplete(r) => r // 2.
          case CFIncomplete(r) => r ++ followUp(s).getOrElse(Set()) // 3.
        }
      }
    }
  }

  private def getSuccNestedLevel(l: List[AST]) = {
    val wsandf = determineTypeOfGroupedOptLists(groupOptListsImplication(groupOptBlocksEquivalence(l)).reverse)
    val succel = getSuccFromList(featureExpr(parentOpt(l.head)), wsandf)

    succel match {
      case CFComplete(r) => r
      case CFIncomplete(r) => r ++ followUp(l.head).getOrElse(Set())
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
    pack[List[AST]]({ (x,y) => parentOpt(x.head).feature.implies(parentOpt(y.head).feature.not).isTautology()})(l.reverse)
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
        else if (f.map(_.not).foldLeft(FeatureExpr.base)(_ and _).isContradiction()) (2, h)::determineTypeOfGroupedOptLists(t)
             else (1, h)::determineTypeOfGroupedOptLists(t)
      }
      case Nil => List()
    }
  }

  // returns a list of previous and next AST elems grouped according to feature expressions
  private def getFeatureGroupedASTElems(s: AST) = {
    val l = prevASTElems(s) ++ nextASTElems(s).drop(1)
    val d = determineTypeOfGroupedOptLists(groupOptListsImplication(groupOptBlocksEquivalence(l))).reverse
    getSuccTailList(s, d)
  }

  // get all succ nodes of o
  private def getNextEqualAnnotatedSucc(o: AST, l: List[(Int, List[List[AST]])]): Option[AST] = {
    if (l.isEmpty) return None
    var el = l.head

    // take tuple with o and examine it
    var il = el._2.filter(_.contains(o)).head.span(_.ne(o))._2.drop(1)
    if (! il.isEmpty) Some(il.head)
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
    var r = Set[AST]()
    for (e <- l) {
      e match {
        case (0, opts) => r = r ++ Set(opts.head.head)
        case (_, opts) => r = r ++ opts.map({ x=> Set(x.head)}).foldLeft(Set[AST]())(_ ++ _)
      }

      if (e._1 == 2 || e._1 == 0) return CFComplete(r)
      if (featureExpr(e._2.head.head).equivalentTo(c) && e._1 == 1) return CFComplete(r)
    }
    CFIncomplete(r)
  }

  // determine recursively all succs
  def getAllSucc(i: AST) = {
    var r = Map[AST, Set[AST]]()
    var s = Set(i)

    while (! s.isEmpty) {
      var c = s.head
      r = r ++ Map((c, succ(c)))
      s = s.drop(1)
      s = s ++ r.getOrElse(c, Set()).diff(r.keys.toSet)
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
  def map2file(m: Map[AST, Set[AST]]) = {
    var dotstring = ""
    val fname = getTmpFileName()
    dotstring += "digraph \"" + fname.getName + "\" {" + "\n"
    dotstring += "node [shape=record];\n"
    for ((o, succs) <- m) {
      val op = esc(PrettyPrinter.print(o))
      dotstring += "\"" + op + "\" [label=\"{{" + op + "}|" + esc(featureExpr(o).toString()) + "}\"];\n"
      for (succ <- succs) dotstring += "\"" + op + "\" -> \"" + esc(PrettyPrinter.print(succ)) + "\"\n"
    }
    dotstring = dotstring + "}\n"
    println(dotstring)
    writeToFile(fname.getAbsolutePath, dotstring)
  }

  private def esc(i: String) = {
    i.replace("\n", "\\\n").replace("{", "\\{").replace("}", "\\}").replace("<", "\\<").replace(">", "\\>")
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


