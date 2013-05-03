package de.fosd.typechef.featureexpr.bdd

import org.sat4j.core.{VecInt, Vec}
import org.sat4j.specs.{IVec, IVecInt}
import java.net.URI
import de.fosd.typechef.featureexpr._
import scala.Predef._

/**
 * the feature model is a special container for a single feature expression
 * that is used very often in a conjunction
 *
 * it stores the formula in an array structure easy to process by the
 * sat solver
 *
 *
 * it can load an expression from a FeatureExpr or from a file in CNF
 * format
 *
 * extra constraints contain arbitrary formulas that are used in every query. may be expensive to calculate each time.
 *
 * assumedFalse and assumedTrue can hold sets of feature names that are known to be true or false (typically from
 * partial configurations). Those are cheap to pass to the SAT solver.
 *
 */
class BDDFeatureModel(val variables: Map[String, Int], val clauses: IVec[IVecInt], val lastVarId: Int,
                      val extraConstraints: BDDFeatureExpr,
                      val assumedFalse: Set[String], val assumedTrue: Set[String]) extends FeatureModel {
    /**
     * make the feature model stricter by a formula
     */
    def and(expr: FeatureExpr /*CNF*/) = {
        new BDDFeatureModel(variables, clauses, lastVarId, CastHelper.asBDDFeatureExpr(extraConstraints and expr), assumedFalse, assumedTrue)
    }


    def assumeTrue(featurename: String) =
        new BDDFeatureModel(variables, clauses, lastVarId, extraConstraints, assumedFalse, assumedTrue + featurename)
    def assumeFalse(featurename: String) =
        new BDDFeatureModel(variables, clauses, lastVarId, extraConstraints, assumedFalse + featurename, assumedTrue)

    /**helper function*/
    def assumptions: BDDFeatureExpr =
        (assumedTrue.map(BDDFeatureExprFactory.createDefinedExternal(_)).fold(BDDFeatureExprFactory.True)(_ and _) and
            assumedFalse.map(BDDFeatureExprFactory.createDefinedExternal(_).not).fold(BDDFeatureExprFactory.True)(_ and _)).asInstanceOf[BDDFeatureExpr]
}

/**
 * empty feature model
 */
object BDDNoFeatureModel extends BDDFeatureModel(Map(), new Vec(), 0, True, Set(), Set())

/**
 * companion object to create feature models
 *
 * TODO: this code is replicated from SATFeatureModel, integrate again
 */
object BDDFeatureModel extends FeatureModelFactory {
    /**
     * create an empty feature model
     */
    def empty: BDDFeatureModel = BDDNoFeatureModel

    /**
     * create a feature model from a feature expression
     */
    def create(expr: FeatureExpr) = BDDNoFeatureModel.and(expr)

    /**
     * create a feature model by loading a CNF file
     * (proprietary format used previously by LinuxAnalysis tools)
     */
    def createFromCNFFile(file: String) = {
        var variables: Map[String, Int] = Map()
        var varIdx = 0
        val clauses = new Vec[IVecInt]()

        for (line <- scala.io.Source.fromFile(file).getLines) {
            if ((line startsWith "@ ") || (line startsWith "$ ")) {
                varIdx += 1
                variables = variables.updated("CONFIG_" + line.substring(2), varIdx)
            } else {
                val vec = new VecInt()
                for (literal <- line.split(" "))
                    vec.push(lookupLiteral(literal, variables))
                clauses.push(vec)
            }

        }
        new BDDFeatureModel(variables, clauses, varIdx, BDDFeatureExprFactory.TrueB, Set(), Set())
    }

    /**
     * load a standard Dimacs file as feature model
     */
    def createFromDimacsFile(file: String, prefix: String = "CONFIG_" , suffix:String="") = {
        var variables: Map[String, Int] = Map()
        val clauses = new Vec[IVecInt]()
        var maxId = 0

        for (line <- scala.io.Source.fromFile(file).getLines) {
            if (line startsWith "c ") {
                val entries = line.substring(2).split(" ")
                val id = if (entries(0) endsWith "$")
                    entries(0).substring(0, entries(0).length - 1).toInt
                else
                    entries(0).toInt
                maxId = scala.math.max(id, maxId)
                variables = variables.updated(prefix + entries(1) + suffix, id)
            } else if ((line startsWith "p ") || (line.trim.size == 0)) {
                //comment, do nothing
            } else {
                val vec = new VecInt()
                for (literal <- line.split(" "))
                    if (literal != "0")
                        vec.push(literal.toInt)
                clauses.push(vec)
            }

        }
        assert(maxId == variables.size)
        new BDDFeatureModel(variables, clauses, maxId, BDDFeatureExprFactory.TrueB, Set(), Set())
    }
    /**
     * special reader for the -2var model used by the LinuxAnalysis tools from waterloo
     */
    def createFromDimacsFile_2Var(file: String) = loadDimacsFile_2Var(scala.io.Source.fromFile(file))
    def createFromDimacsFile_2Var(file: URI) = loadDimacsFile_2Var(scala.io.Source.fromFile(file))

    private def loadDimacsFile_2Var(source: scala.io.Source) = {
        var variables: Map[String, Int] = Map()
        val clauses = new Vec[IVecInt]()
        var maxId = 0

        for (line <- source.getLines) {
            if (line startsWith "c ") {
                val entries = line.substring(2).split(" ")
                val id = if (entries(0) endsWith "$")
                    entries(0).substring(0, entries(0).length - 1).toInt
                else
                    entries(0).toInt
                maxId = scala.math.max(id, maxId)
                val varname = "CONFIG_" + (/*if (entries(1).endsWith("_m")) entries(1).substring(0, entries(1).length - 2)+"_MODULE" else*/ entries(1))
                if (variables contains varname)
                    assert(false, "variable " + varname + " declared twice")
                variables = variables.updated(varname, id)
            } else if ((line startsWith "p ") || (line.trim.size == 0)) {
                //comment, do nothing
            } else {
                val vec = new VecInt()
                for (literal <- line.split(" "))
                    if (literal != "0")
                        vec.push(literal.toInt)
                clauses.push(vec)
            }

        }
        assert(maxId == variables.size)
        new BDDFeatureModel(variables, clauses, maxId, BDDFeatureExprFactory.TrueB, Set(), Set())
    }

    private def lookupLiteral(literal: String, variables: Map[String, Int]) =
        if (literal.startsWith("-"))
            -variables.getOrElse("CONFIG_" + (literal.substring(1)), throw new Exception("variable not declared"))
        else
            variables.getOrElse("CONFIG_" + literal, throw new Exception("variable not declared"))


}
