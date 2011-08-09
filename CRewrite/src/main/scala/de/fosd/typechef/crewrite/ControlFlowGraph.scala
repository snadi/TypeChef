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
        var p = prevOpts(o)
        if (p.isEmpty) Set(o.parent)
        else iterateOpts(o, eqOptLists(p).map(_.head))
      }
    }

  val succ: Attributable ==> Set[Attributable] =
    attr {
      case o@Opt(_, _) => {
        var p = nextOpts(o)
        if (p.isEmpty) Set(o.parent.next)
        else iterateOpts(o, eqOptLists(p).map(_.head))
      }
    }

  val isPartOfIEEChain: Attributable ==> Boolean = {
    attr {
      case o@Opt(_, _) => {
        if (o.feature.equivalentTo(FeatureExpr.base)) false
        else {
          var p = prevOpts(o)
          var n = nextOpts(o)
          p = p.takeWhile({ e => o.feature.implies(e.feature).isTautology() == false})
          n = n.takeWhile({ e => o.feature.implies(e.feature).isTautology() == false})
          val r = p ++ List(o) ++ n
          r.map(_.feature).foldLeft(FeatureExpr.dead)(_ or _).isTautology()
        }
      }
    }
  }

  private def iterateOpts(o: Opt[_], s: List[Opt[_]]): Set[Attributable] = {
    var l = List[Opt[_]]()
    for (e <-s) {
      if (o.feature.implies(e.feature).isTautology()) return Set(e)
      else {
        l = l.:+(e)
        if (l.map(_.feature).foldLeft(FeatureExpr.dead)(_ or _).isTautology())
          return l.map(Set(_)).foldLeft(Set[Attributable]())(_ ++ _)
      }
    }
    Set[Attributable]()
  }

  private def eqOptLists(l: List[Opt[_]]): List[List[Opt[_]]] = {
    var r = List[List[Opt[_]]]()
    var cl = List[Opt[_]]()
    for (o <- l) {
      if (cl.isEmpty) { cl = cl.+:(o) }
      else {
        if (cl.head.feature.equivalentTo(o.feature)) cl = cl.+:(o)
        else {
          r = r.:+(cl)
          cl = List(o)
        }
      }
    }
    r = r.:+(cl)
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
      case InitDeclaratorI(declarator, _, Some(i)) => declarator->uses// ++ i->uses
      case AtomicNamedDeclarator(_, id, _) => Set(id)
      case NestedNamedDeclarator(_, nestedDecl, _) => nestedDecl->uses
      case Initializer(_, expr) => expr->uses
      case Id(name) => Set(Id(name))
      case Constant(_) => Set()
      case StringLit(_) => Set()
      case SimplePostfixSuffix(_) => Set()
      case PointerPostfixSuffix(kind, id) => Set(id)
      case FunctionCall(params) => Set() // TODO List[Opt[Expr]]
      case ArrayAccess(expr) => expr->uses
      case PostfixExpr(p, s) => p->uses// ++ s->uses
      case UnaryExpr(_, e) => e->uses
      case SizeOfExprT(_) => Set()
      case SizeOfExprU(expr) => expr->uses
      case CastExpr(_, expr) => expr->uses
      case PointerDerefExpr(castExpr) => castExpr->uses
      case PointerCreationExpr(castExpr) => castExpr->uses
      case UnaryOpExpr(kind, castExpr) => castExpr->uses
      case NAryExpr(e, others) => e->uses // TODO others List[Opt[NarySubExpr]]
      case NArySubExpr(_, e) => e->uses
      case ConditionalExpr(condition, Some(thenExpr), elseExpr) => condition->uses //++ thenExpr->uses ++ elseExpr->uses
      case AssignExpr(target, _, _) => target->uses
      case ExprList(_) => Set()
      case _ => Set()
    }


  val defines: Attributable ==> Set[Id] =
    attr {
      case CompoundStatement(innerStatements) => innerStatements.map(defines).foldLeft(Set[Id]())(_ ++ _)
      case DeclarationStatement(decl) => decl->defines
      case Declaration(_, init) => init.map(defines).foldLeft(Set[Id]())(_ ++ _)
      case InitDeclaratorI(declarator, _, _) => declarator->defines
      case AtomicNamedDeclarator(_, id, _) => Set(id)
      case WhileStatement(_, s) => s->defines
      case DoStatement(_, s) => s->defines
      case ForStatement(_, _, _, s) => s->defines
      case CaseStatement(_, Some(s)) => s->defines
      case DefaultStatement(Some(s)) => s->defines
      case SwitchStatement(_, s) => s->defines
      case IfStatement(_, thenBranch, elifs, elseBranch) => {
          var r = thenBranch->defines ++ elifs.map(defines).foldLeft(Set[Id]())(_ ++ _)
          if (elseBranch.getOrElse("") != "")
            r ++ defines(elseBranch.get)
          else
            r
      }
      case ElifStatement(_, thenBranch) => thenBranch->childAST->defines
      case o@Opt(_, _) => o->childAST->defines
      case o@One(_) => o->childAST->defines
      case c@Choice(_, thenBranch, elseBranch) => defines(thenBranch) ++ defines(elseBranch)
      case _ => Set()
    }
}


