package de.fosd.typechef.conditional

import org.kiama.attribution.Attribution._
import org.kiama._
import attribution.Attributable
import de.fosd.typechef.featureexpr._

/**
 * Simplified navigation support
 *
 * parentOpt, prevOpt, and nextOpt provide navigation between
 * Attributable nodes
 */
trait ConditionalNavigation extends FeatureExprLookup {

  val parentOpt: Attributable ==> Opt[_] = attr { case a: Attributable => findParentOpt(a)}
  private def findParentOpt(a: Attributable): Opt[_] =
    a.parent match {
      case o: Opt[_] => o
      case c: Conditional[_] => Conditional.toOptList(c).head
      case a: Attributable => findParentOpt(a)
      case _ => null
    }

  val prevOpt: Attributable ==> Opt[_] = attr {case o@Opt(_, _) => findPrevEqualOpt(o, o.feature)}
  private def findPrevEqualOpt(a: Attributable, f: FeatureExpr): Opt[_] = {
    a.prev[Attributable] match {
      case o@Opt(_, _) if (f.equivalentTo(o.feature)) => o
      case o@Opt(_, _) if (!f.equivalentTo(o.feature)) => findPrevEqualOpt(o, f)
      case _ => null
    }
  }

  val prevOpts: Opt[_] ==> List[Opt[_]] = {case o@Opt(_, _) => getPrevOpts(o)}
  private def getPrevOpts(a: Opt[_]): List[Opt[_]] = {
    a.prev[Attributable] match {
      case o@Opt(_, _) => getPrevOpts(o) ++ List(o)
      case null => Nil
    }
  }

  val nextOpts: Opt[_] ==> List[Opt[_]] = {case o@Opt(_, _) => getNextOpts(o)}
  private def getNextOpts(a: Opt[_]): List[Opt[_]] = {
    a.next[Attributable] match {
      case o@Opt(_, _) => o :: getNextOpts(o)
      case null => Nil
    }
  }

  val nextOpt: Attributable ==> Opt[_] = attr {case o@Opt(_,_) => findNextEqualOpt(o, o.feature)}
  private def findNextEqualOpt(a: Attributable, f: FeatureExpr): Opt[_] = {
    a.next[Attributable] match {
      case o@Opt(_, _) if (f.equivalentTo(o.feature)) => o
      case o@Opt(_, _) if (!f.equivalentTo(o.feature)) => findNextEqualOpt(o, f)
      case _ => null
    }
  }

  val isVariable: Attributable ==> Boolean = attr {
    case a => featureExpr(a).not.isContradiction()
  }

  private def lastChoice[T <: Attributable](x: Choice[T]): T =
    x.elseBranch match {
      case c: Choice[T] => lastChoice(c)
      case One(c) => c
    }

  private def firstChoice[T <: Attributable](x: Choice[T]): T =
    x.thenBranch match {
      case c: Choice[T] => firstChoice(c)
      case One(c) => c
    }
}

