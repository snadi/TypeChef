package de.fosd.typechef.lexer.options

import de.fosd.typechef.featureexprJava._
import de.fosd.typechef.featureexprInterface._
import de.fosd.typechef.featureexprUtil._

/**
 * Created by IntelliJ IDEA.
 * User: kaestner
 * Date: 12.02.12
 * Time: 11:36
 * To change this template use File | Settings | File Templates.
 */
class PartialConfiguration {
    def this(definedFeatures: Array[String], undefinedFeatures: Array[String], fexpr: AbstractFeatureExprModule#AbstractFeatureExpr) {
        this()
        this.`def` = definedFeatures
        this.undef = undefinedFeatures
        this.fexpr = fexpr
    }

    def getDefinedFeatures: Array[String] = {
        return `def`
    }

    def getUndefinedFeatures: Array[String] = {
        return undef
    }

    def getFeatureExpr: AbstractFeatureExprModule#AbstractFeatureExpr = {
        return fexpr
    }

    private var `def`: Array[String] = null
    private var undef: Array[String] = null
    private var fexpr: AbstractFeatureExprModule#AbstractFeatureExpr = null
}
