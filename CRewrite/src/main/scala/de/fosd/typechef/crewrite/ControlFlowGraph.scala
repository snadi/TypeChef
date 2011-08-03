package de.fosd.typechef.crewrite

import org.kiama._
import org.kiama.attribution.DynamicAttribution._
import de.fosd.typechef.parser.c._

// http://code.google.com/p/kiama/source/browse/src/org/kiama/example/dataflow/Dataflow.scala
trait ControlFlow {
  val succ: Statement ==> Set[Statement]
  val following: Statement ==> Set[Statement]
}

trait ControlFlowImpl extends ControlFlow {
  val succ: Statement ==> Set[Statement] =
    attr {
      case CompoundStatement(h :: _) => Set(h)
      case EmptyStatement() => Set()
      case IfStatement(_, thenBranch, elifs, elseBranch) => Set(thenBranch, elifs, elseBranch)
      case t @ WhileStatement(_, s) => t->following + s
    }

  val following: Statement => Set[Statement] =
    childAttr {
      case s => {
        case t @ IfStatement(_, _, _, _) => t->following
        case t @ WhileStatement(_, _) => Set(t)
        case b: CompoundStatement if s isLast => b->following
        case CompoundStatement(_) => Set(s.next)
        case _ => Set()
      }
    }
}

