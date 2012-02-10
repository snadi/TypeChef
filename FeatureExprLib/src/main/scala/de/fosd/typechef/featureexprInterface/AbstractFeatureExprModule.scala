package de.fosd.typechef.featureexprInterface

import de.fosd.typechef.featureexprUtil._
import java.io.Writer


/**
 * Define the interface of a Feature Expression Library.
 * The pattern used here imitates Standard ML modules. See Programming in Scala, Chap. 29.
 * User: pgiarrusso
 * Date: 9/2/2012
 */
trait AbstractFeatureExprModule {


    //Define a (bounded) abstract type member
    type FeatureExpr <: AbstractFeatureExpr

    trait AbstractFeatureExpr {
        //Use the abstract FeatureExpr here. Note that when FeatureExpr is refined, the accepted type here is refined as
        //well - covariantly. See Programming in Scala, Sec. 20.6. The same applies within AbstractFEFactory below.

        /**
         * x.isSatisfiable(fm) is short for x.and(fm).isSatisfiable
         * but is faster because FM is cached
         */
        def isSatisfiable(fm: FeatureModel): Boolean
        protected def calcSize: Int
        def toTextExpr: String //or other ToString variations for debugging etc
        protected def collectDistinctFeatures: Set[String]

        def or(that: FeatureExpr): FeatureExpr
        def and(that: FeatureExpr): FeatureExpr
        def not(): FeatureExpr


        //equals, hashcode


        def unary_! = not
        def &(that: FeatureExpr) = and(that)
        def |(that: FeatureExpr) = or(that)

        def implies(that: FeatureExpr) = this.not.or(that)
        def xor(that: FeatureExpr) = (this or that) andNot (this and that)
        def equiv(that: FeatureExpr) = (this and that) or (this.not and that.not)

        def orNot(that: FeatureExpr) = this or (that.not)
        def andNot(that: FeatureExpr) = this and (that.not)
        def mex(that: FeatureExpr): FeatureExpr = (this and that).not

        def isContradiction(): Boolean = isContradiction(NoFeatureModel)
        def isTautology(): Boolean = isTautology(NoFeatureModel)
        def isDead(): Boolean = isContradiction(NoFeatureModel)
        def isBase(): Boolean = isTautology(NoFeatureModel)
        def isSatisfiable(): Boolean = isSatisfiable(NoFeatureModel)
        /**
         * FM -> X is tautology if FM.implies(X).isTautology or
         * !FM.and.(x.not).isSatisfiable
         *
         **/
        def isTautology(fm: FeatureModel): Boolean = !this.not.isSatisfiable(fm)
        def isContradiction(fm: FeatureModel): Boolean = !isSatisfiable(fm)


        /**
         * uses a SAT solver to determine whether two expressions are
         * equivalent.
         *
         * for performance reasons, it checks pointer
         * equivalence first, but won't use the recursive equals on aexpr
         * (there should only be few cases when equals is more
         * accurate than eq, which are not worth the performance
         * overhead)
         */
        def equivalentTo(that: FeatureExpr): Boolean = (this eq that) || (this equiv that).isTautology();
        def equivalentTo(that: FeatureExpr, fm: FeatureModel): Boolean = (this eq that) || (this equiv that).isTautology(fm);

        protected def indent(level: Int): String = "\t" * level

        final lazy val size: Int = calcSize

        //        /**
        //         * heuristic to determine whether a feature expression is small
        //         * (may be used to decide whether to inline it or not)
        //         *
        //         * use with care
        //         */
        //        def isSmall(): Boolean = size <= 10

        //    /**
        //     * map function that applies to all leafs in the feature expression (i.e. all DefinedExpr nodes)
        //     */
        //    def mapDefinedExpr(f: DefinedExpr => FeatureExpr, cache: Map[FeatureExpr, FeatureExpr]): FeatureExpr

        /**
         * Converts this formula to a textual expression.
         */
        override def toString: String = toTextExpr


        /**
         * Prints the textual representation of this formula on a Writer. The result shall be equivalent to
         * p.print(toTextExpr), but it should avoid consuming so much temporary space.
         * @param p the output Writer
         */
        def print(p: Writer) = p.write(toTextExpr)
        def debug_print(indent: Int): String = toTextExpr


//        /**
//         * simple translation into a FeatureExprValue if needed for some reason
//         * (creates IF(expr, 1, 0))
//         */
//        def toFeatureExprValue: FeatureExprValue =
//            FeatureExprFactory.createIf(this, FeatureExprFactory.createValue(1l), FeatureExprFactory.createValue(0l))

    }


    trait AbstractFeatureExprFactory extends FeatureExprTreeFactory {
        def createDefinedExternal(v: String): FeatureExpr
        def createDefinedMacro(name: String, macroTable: FeatureProvider): FeatureExpr

        def base: FeatureExpr
        def dead: FeatureExpr
        def True = base
        def False = dead
    }

    def FeatureExprFactory: AbstractFeatureExprFactory

    def ErrorFeature(msg: String): FeatureExpr

}
