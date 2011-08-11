package de.fosd.typechef.crewrite

import org.kiama._
import org.kiama.attribution.Attributable
import org.kiama.attribution.DynamicAttribution._
import de.fosd.typechef.parser.c._
import de.fosd.typechef.conditional._
import de.fosd.typechef.featureexpr._

// http://code.google.com/p/kiama/source/browse/src/org/kiama/example/dataflow/Dataflow.scala
trait ControlFlow {
  val pred: Attributable ==> Set[Attributable]
  val succ: Attributable ==> Set[Attributable]
  val following: Attributable ==> Set[Attributable]
}

trait ControlFlowImpl extends ControlFlow with ASTNavigation with ConditionalNavigation with FeatureExprLookup {
  val pred: Attributable ==> Set[Attributable] =
    attr {
      case o@Opt(_, _) => {
        Set[Attributable]()
      }
    }

  val succ: Attributable ==> Set[Attributable] =
    attr {
      case o@Opt(_, _) => {
        val l = prevOpts(o) ++ List(o) ++ nextOpts(o)
        getSuccs(o, determineTypeOfOptLists(groupOptListsImplication(groupOptBlocksEquivalence(l))).reverse)
      }
    }

  val following: Attributable ==> Set[Attributable] =
    attr {
      case o@Opt(_, _) => {
        Set[Attributable]()
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
  private def groupOptBlocksEquivalence(l: List[Opt[_]]) = {
    pack[Opt[_]](_.feature equivalentTo _.feature)(l)
  }

  // group List[Opt[_]] according to implication
  // later one should imply the not of previous ones; therefore using l.reverse
  private def groupOptListsImplication(l: List[List[Opt[_]]]) = {
    pack[List[Opt[_]]]({ (x,y) => x.head.feature.implies(y.head.feature.not).isTautology()})(l.reverse)
  }

  // get type of List[List[Opt[_]]:
  // 0 -> only true values
  // 1 -> #if-(#elif)* block
  // 2 -> #if-(#elif)*-#else block
  private def determineTypeOfOptLists(l: List[List[List[Opt[_]]]]): List[(Int, List[List[Opt[_]]])] = {
    l match {
      case (h::t) => {
        val f = h.map(_.head.feature)
        if (f.foldLeft(FeatureExpr.base)(_ and _).isTautology()) (0, h)::determineTypeOfOptLists(t)
        else if (f.map(_.not).foldLeft(FeatureExpr.base)(_ and _).isContradiction()) (2, h)::determineTypeOfOptLists(t)
             else (1, h)::determineTypeOfOptLists(t)
      }
      case Nil => List()
    }
  }

  // similar to takeWhile except it takes one element more
  private def takeWhileFollow[T](p: (T) => Boolean, l: List[T]): List[T] = {
    l.takeWhile(p) ++ l.dropWhile(p).take(1)
  }

  // get all succ nodes of o
  private def getSuccs(o: Opt[_], l: List[(Int, List[List[Opt[_]]])]): Set[Attributable] = {
    var r = Set[Attributable]()

    // get the list with o and all following lists
    // iterate each sublist of the incoming tuples (Int, List[List[Opt[_]]] combine equality check
    // with foldLeft and drop tuples in which o does not occur
    var rl = l.dropWhile(_._2.map(_.map(_.eq(o)).foldLeft(false)(_ || _)).foldLeft(false)(_ || _).unary_!)
    var el = rl.head
    rl = rl.drop(1)

    // take all if blocks plus the next one
    rl = takeWhileFollow[(Int, List[List[Opt[_]]])](_._1.==(1), rl)

    // take tuple with o and examine it
    var il = el._2.filter(_.contains(o)).head.span(_.ne(o))._2.drop(1)

    if (! il.isEmpty) r = r ++ Set(il.head)
    //else if (rl.isEmpty) r = r ++ succ(o.parent)
    else {
      for (e <- rl) {
        e match {
          case (0, opts) => r = r ++ Set(opts.head.head)
          case (_, opts) => r = r ++ opts.map({ x => Set(x.head)}).foldLeft(Set[Attributable]())(_ ++ _)
        }
      }
    }
    r
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


