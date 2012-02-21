package de.fosd
package typechef.featureexprJava
import typechef.featureexpr
import typechef.featureexprUtil.{FeatureExprTreeFactory, FeatureExprValue, FeatureExprTree}

/*
import typechef.featureexprInterface.AbstractFeatureExprModule

trait FeatureExpr extends AbstractFeatureExprModule#AbstractFeatureExpr
*/

object FeatureExprLibScala {
    val base = featureexpr.FeatureExpr.base
    val dead = featureexpr.FeatureExpr.dead

    val NoFeatureModel = featureexpr.NoFeatureModel
    val ml = featureexpr.FeatureModelLoader
    val l = featureexpr.FeatureExpr
    def zero = l.createInteger(0)
    //TODO: for some reason FeatureExprTree<Long> does no longer work since
    //scala 2.9. We use FeatureExprTree<Object>, which seems unproblematic since
    //the result is not constructed or inspected in Java code anyway, but only passed around

    //However, here in Scala we must use Long!
    def toFeatureExpr(v: FeatureExprTree[_]) = FeatureExprValue.toFeatureExpr(v.asInstanceOf[FeatureExprTree[Long]])
}
