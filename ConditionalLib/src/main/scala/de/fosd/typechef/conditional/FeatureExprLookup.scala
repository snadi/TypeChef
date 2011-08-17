package de.fosd.typechef.conditional

import org.kiama.attribution.Attribution._
import org.kiama._
import attribution.Attributable
import de.fosd.typechef.featureexpr.FeatureExpr

/**
 * provides featureExpr lookup for *all* AST nodes
 */
trait FeatureExprLookup {

  val featureExpr: Attributable ==> FeatureExpr = attr {
    case node =>
      node.parent match {
        case o@Opt(f, _) => featureExpr(o) and f
        case c: Choice[_] => featureExpr(c) and (if (c.thenBranch == node) c.feature else c.feature.not)
        case null => FeatureExpr.base
        case e => featureExpr(e)
      }
  }

  // all featureExpr from to root to the node
  val featureExprList: Attributable ==> List[FeatureExpr] = attr {
    case node =>
      node.parent match {
        case o@Opt(f, _) => featureExprList(o) ++ List(f)
        case c: Choice[_] => featureExprList(c) ++ (if (c.thenBranch == node) List(c.feature) else List(c.feature.not))
        case null => List()
        case e => featureExprList(e)
      }
  }
}

