package de.fosd.typechef

package object featureexprUtil {

    type FeatureExprValue = FeatureExprTree[Long]


    class FeatureException(msg: String) extends RuntimeException(msg)

    class FeatureArithmeticException(msg: String) extends FeatureException(msg)

}
