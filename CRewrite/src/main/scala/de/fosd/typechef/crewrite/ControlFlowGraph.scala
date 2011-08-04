package de.fosd.typechef.crewrite

import org.kiama._
import org.kiama.attribution.Attributable
import org.kiama.attribution.DynamicAttribution._
import de.fosd.typechef.parser.c._
import de.fosd.typechef.conditional._
import de.fosd.typechef.featureexpr._

// http://code.google.com/p/kiama/source/browse/src/org/kiama/example/dataflow/Dataflow.scala
trait ControlFlow {
  val succ: Statement ==> Set[Opt[Statement]]
  val following: Statement ==> Set[Opt[Statement]]
}

trait ControlFlowImpl extends ControlFlow {
  val succ: Statement ==> Set[Opt[Statement]] =
    attr {
      case CompoundStatement(h :: _) => Set(h)
      case EmptyStatement() => Set()
      case IfStatement(_, thenBranch, elifs, elseBranch) => Conditional.toOptSet(thenBranch)
      case t @ WhileStatement(_, s) => t->following ++ Conditional.toOptSet(s)
    }

  val following: Statement ==> Set[Opt[Statement]] =
    childAttr {
      case s => {
        case t @ IfStatement(_, _, _, _) => t->following
        case t @ WhileStatement(_, _) => Set(Opt(FeatureExpr.base, t))
        case b: CompoundStatement if s isLast => b->following
        case CompoundStatement(_) => Set(s.next)
        case _ => Set()
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
      case o@Opt(_, _) => o->childAST->defines
      case _ => Set()
    }
}


