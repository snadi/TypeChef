package de.fosd.typechef.featureexpr.sat

import de.fosd.typechef.featureexpr.FeatureExprParser
import java.io.File

/**
 * Created with IntelliJ IDEA.
 * User: snadi
 * Date: 26/02/13
 * Time: 4:44 PM
 * To change this template use File | Settings | File Templates.
 */
object SarahTest extends App {

    val featureParser = new FeatureExprParser()
    println("reading formula")
    val featureExpr = featureParser.parseFile("BIG_FORMULA.txt")
    println("read formula")
    SATFeatureModel.create(featureExpr).asInstanceOf[SATFeatureModel].writeToDimacsFile(new File("BIG_FORMULA.dimacs"))

}
