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
        Set[Attributable]()
      }
    }

  // adding a new element to a list of lists according to function f
  // function f is applied to the first element of the first list of l and the element e itself
  // if true add e to that list
  // if false prepend a new list to l
  private def elem2list[T](f: (T, T) => Boolean)(l: List[List[T]])(e: T): List[List[T]] = {
    if (l.head.isEmpty) List(List(e))
    else {
      if (f(e, l.head.head)) (e::l.head)::l.tail
      else List(e)::l
    }
  }

  // group consecutive Opts in a list and return a list of list containing consecutive (feature equivalent) opts
  private def groupOptBlocks(l: List[Opt[_]]) = {
    val nl = List[List[Opt[_]]](List())
    l.foldLeft(nl)(elem2list[Opt[_]](_.feature equivalentTo _.feature)(_)(_))
  }

  // group the incoming list (list of equivalent opt blocks) into #if and #if-else blocks
  private def groupIfIfelseBlocks(l: List[List[Opt[_]]]) = {
    var m = List[List[Opt[_]]]()
    var r = List[(Int, List[Opt[_]])]()
    for (e <- l) {
      if (m.size == 0) m = m.:+(e)
      else {
        if (m.head.head.feature.implies(e.head.feature.not()).isTautology()) {
          m = m.:+(e)
          if (m.map(_.head.feature.not()).foldLeft(FeatureExpr.base)(_ and _).isContradiction()) {
            r = r.:+((2, m.flatten))
            m = List()
          }
        }
        else {
          if (m.map(_.head.feature).foldLeft(FeatureExpr.base)(_ and _).isTautology()) r = r.:+((0, m.flatten))
          else r = r.:+((1, m.flatten))
          m = List(e)
        }
      }
    }
    r = r.:+((1, m.flatten))
    r
  }

  private def detSucc(o: Opt[_], l: List[(Int, List[Opt[_]])]) = {
    var r = Set[Attributable]()

    r
  }

  val mysucc: Attributable ==> List[(Int, List[Opt[_]])] = {
    attr {
      case o@Opt(_, _) => {
        val l = prevOpts(o) ++ List(o) ++ nextOpts(o)
        val m = groupOptBlocks(l)
        val n = groupIfIfelseBlocks(m)
        n
      }
    }
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


