package de.fosd.typechef.featureexprImpl.bdd

import java.io.Writer
import net.sf.javabdd._
import de.fosd.typechef.featureexprUtil._
import de.fosd.typechef.featureexprInterface.AbstractFeatureExprModule
import collection.mutable.{WeakHashMap, Map}

object BDDFeatureExprLibrary extends AbstractFeatureExprModule {

    type FeatureExpr = BDDFeatureExprImpl
    val FeatureExpr = BDDFeatureExprFactory

    type FeatureModel = BDDFeatureModel
    val FeatureModelLoader = BDDFeatureModelLoader
    val NoFeatureModel: FeatureModel = BDDNoFeatureModel


    /**
     * External interface for construction of non-boolean feature expressions
     * (mostly delegated to BDDFExprBuilder )
     *
     * Also provides access to the primitives base and dead (for true and false)
     * and allows to create DefinedExternal nodes
     */
    object BDDFeatureExprFactory extends AbstractFeatureExprFactory {


        def createDefinedExternal(name: String): FeatureExpr = BDDFExprBuilder.definedExternal(name)
        def createDefinedMacro(name: String, macroTable: FeatureProvider): FeatureExpr = BDDFExprBuilder.definedMacro(name, macroTable)


        //helper
        //        def createIf(condition: FeatureExpr, thenBranch: FeatureExpr, elseBranch: FeatureExpr): FeatureExpr = FeatureExprFactory.createBooleanIf(condition, thenBranch, elseBranch)

        val base: FeatureExpr = TrueImpl
        val dead: FeatureExpr = FalseImpl
    }


    /**
     * Propositional (or boolean) feature expressions.
     *
     * Feature expressions are compared on object identity (comparing them for equivalence is
     * an additional but expensive operation). Connectives such as "and", "or" and "not"
     * memoize results, so that the operation yields identical results on identical parameters.
     * Classes And, Or and Not are made package-private, and their constructors wrapped
     * through companion objects, to prevent the construction of formulas in any other way.
     *
     * However, this is not yet enough to guarantee the 'maximal sharing' property, because the
     * and/or operators are also associative, but the memoization cannot be associative.
     * Papers on hash-consing explain that one needs to perform a further normalization step.
     *
     * More in general, one can almost prove a theorem called the weak-canonicalization guarantee:
     *
     * If at a given time during program execution, two formula objects represent structurally
     * equal formulas, i.e. which are deeply equal modulo the order of operands of "and" and "or",
     * then they are represented by the same object.
     *
     * XXX: HOWEVER, that the associative property does not hold with pointer equality:
     * (a and b) and c ne a and (b and c). Hopefully this is fixable through different caching.
     *
     * Note that this is not related to formula equivalence, rather to pointer equality.
     * This does not hold for formulas existing at different moments, because caches
     * are implemented using weak references, so if a formula disappears from the heap
     * it is recreated. However, this is not observable for the code.
     *
     * The weak canonicalization property, if true, should allows also ensuring the strong-canonicalization guarantee:
     * If at a given time during program execution, two formula objects a and b
     * represent equivalent formulas, then a.toCNF eq b.toCNF (where eq denotes pointer equality).
     *
     * CNF canonicalization, by construction, ensures that a.toCNF and b.toCNF are structurally equal.
     * The weak canonicalization property would also ensure that they are the same object.
     *
     * It would be interesting to see what happens for toEquiCNF.
     */
    class BDDFeatureExprImpl(private[BDDFeatureExprLibrary] val bdd: BDD) extends AbstractFeatureExpr {

        def or(that: FeatureExpr): FeatureExpr = BDDFExprBuilder.or(this, that)
        def and(that: FeatureExpr): FeatureExpr = BDDFExprBuilder.and(this, that)
        def not(): FeatureExpr = BDDFExprBuilder.not(this)


        override def implies(that: FeatureExpr) = BDDFExprBuilder.imp(this, that)
        override def xor(that: FeatureExpr) = BDDFExprBuilder.xor(this, that)
        override def equiv(that: FeatureExpr) = BDDFExprBuilder.biimp(this, that)

        private val cacheIsSatisfiable: WeakHashMap[FeatureModel, Boolean] = WeakHashMap()
        def isSatisfiable(fm: FeatureModel): Boolean =
            if (bdd.isOne) true //assuming a valid feature model
            else if (bdd.isZero) false
            else if (fm == NoFeatureModel || fm == null) bdd.satOne() != BDDFExprBuilder.FALSE
            //combination with a small FeatureExpr feature model
            else if (fm.clauses.isEmpty) (bdd and fm.extraConstraints.asInstanceOf[BDDFeatureExprImpl].bdd).satOne() != BDDFExprBuilder.FALSE
            //combination with SAT
            else cacheIsSatisfiable.getOrElseUpdate(fm,
                SatSolver.isSatisfiable(fm, toDnfClauses(toScalaAllSat((bdd and fm.extraConstraints.asInstanceOf[BDDFeatureExprImpl].bdd).not().allsat())), BDDFExprBuilder.lookupFeatureName)
            )

        final override def equals(that: Any) = that match {
            case x: FeatureExpr => bdd.equals(x.bdd)
            case _ => super.equals(that)
        }
        override def hashCode = bdd.hashCode

        protected def calcSize: Int = bddAllSat.foldLeft(0)(_ + _.filter(_ >= 0).size)

        /**
         * Converts this formula to a textual expression.
         */
        def toTextExpr: String =
            printbdd(bdd, "1", "0", " && ", " || ", i => "definedEx(" + BDDFExprBuilder.lookupFeatureName(i) + ")")
        override def toString: String =
            printbdd(bdd, "True", "False", " & ", " | ", BDDFExprBuilder.lookupFeatureName(_))

        private def printbdd(bdd: BDD, one: String, zero: String, and: String, or: String, toName: (Int) => String): String =
            if (bdd.isOne()) one
            else if (bdd.isZero()) zero
            else {
                def clause(d: Array[Byte]): String = d.zip(0 to (d.length - 1)).filter(_._1 >= 0).map(
                    x => (if (x._1 == 0) "!" else "") + toName(x._2)
                ).mkString(and)

                return bddAllSat.map(clause(_)).mkString(or)
            }

        private def bddAllSat: Iterator[Array[Byte]] = toScalaAllSat(bdd.allsat())

        private def toScalaAllSat(allsat: java.util.List[_]): Iterator[Array[Byte]] =
            scala.collection.JavaConversions.asScalaIterator(allsat.asInstanceOf[java.util.List[Array[Byte]]].iterator())

        /**
         * input allsat format
         *
         * output clausel format with sets of variable ids (negative means negated)
         */
        private def toDnfClauses(allsat: Iterator[Array[Byte]]): Iterator[Seq[Int]] = {
            def clause(d: Array[Byte]): Seq[Int] = d.zip(0 to (d.length - 1)).filter(_._1 >= 0).map(
                x => (if (x._1 == 0) -1 else 1) * x._2
            )
            allsat.map(clause(_))
        }


        //        /**
        //         * simple translation into a FeatureExprValue if needed for some reason
        //         * (creates IF(expr, 1, 0))
        //         */
        //        def toFeatureExprValue: FeatureExprValue =
        //            FeatureExprFactory.createIf(this, FeatureExprFactory.createValue(1l), FeatureExprFactory.createValue(0l))


        /**
         * helper function for statistics and such that determines which
         * features are involved in this feature expression
         */
        private def collectDistinctFeatureIds: Set[Int] =
            bddAllSat.flatMap(clause => clause.zip(0 to (clause.length - 1)).filter(_._1 >= 0).map(_._2)).toSet

        def collectDistinctFeatures: Set[String] =
            collectDistinctFeatureIds.map(BDDFExprBuilder lookupFeatureName _)


        /**
         * counts the number of features in this expression for statistic
         * purposes
         */
        def countDistinctFeatures: Int = collectDistinctFeatureIds.size
    }


    override def ErrorFeature(msg: String): FeatureExpr = ErrorFeatureImpl(msg)

    //
    //// XXX: this should be recognized by the caller and lead to clean termination instead of a stack trace. At least,
    //// however, this is only a concern for erroneous input anyway (but isn't it our point to detect it?)
    case class ErrorFeatureImpl(msg: String) extends FeatureExpr(BDDFExprBuilder.FALSE) {
        private def error: Nothing = throw new FeatureArithmeticException(msg)
        override def toTextExpr = error
        //    override def mapDefinedExpr(f: DefinedExpr => FeatureExpr, cache: Map[FeatureExpr, FeatureExpr]) = error
        override def debug_print(x: Int) = error
    }


    ////////////////////////////
    // propositional formulas //
    ////////////////////////////
    /**
     * True and False. They are represented as special cases of And and Or
     * with no clauses, as it is common practice, for instance in the resolution algorithm.
     *
     * True is the zero element for And, while False is the zero element for Or.
     * Therefore, True can be represented as an empty conjunction, while False
     * by an empty disjunction.
     *
     * One can imagine to build a disjunction incrementally, by having an empty one
     * be considered as false, so that each added operand can make the resulting
     * formula true. Dually, an empty conjunction is true, and and'ing clauses can
     * make it false.
     *
     * This avoids introducing two new leaf nodes to handle (avoiding some bugs causing NoLiteralException).
     *
     * Moreover, since those are valid formulas, they would otherwise be valid but
     * non-canonical representations, and avoiding the very existence of such things
     * simplifies ensuring that our canonicalization algorithms actually work.
     *
     * The use of only canonical representations for True and False is ensured thanks to the
     * apply methods of the And and Or companion objects, which convert any empty set of
     * clauses into the canonical True or False object.
     */
    object TrueImpl extends BDDFeatureExprImpl(BDDFExprBuilder.TRUE) with DefaultPrint {
        override def toString = "True"
        override def toTextExpr = "1"
        override def debug_print(ind: Int) = indent(ind) + toTextExpr + "\n"
        override def isSatisfiable(fm: FeatureModel) = true
    }

    object FalseImpl extends BDDFeatureExprImpl(BDDFExprBuilder.FALSE) with DefaultPrint {
        override def toString = "False"
        override def toTextExpr = "0"
        override def debug_print(ind: Int) = indent(ind) + toTextExpr + "\n"
        override def isSatisfiable(fm: FeatureModel) = false
    }

    trait DefaultPrint extends BDDFeatureExprImpl {
        override def print(p: Writer) = p.write(toTextExpr)
    }


    /**
     * Central builder class, responsible for simplification of expressions during creation
     * and for extensive caching.
     */
    private object BDDFExprBuilder {


        val bddCacheSize = 100000
        var bddValNum = 100000
        var bddVarNum = 100
        var maxFeatureId = -1
        val bddFactory = BDDFactory.init(bddValNum, bddCacheSize)
        bddFactory.setVarNum(bddVarNum)

        val TRUE: BDD = bddFactory.one()
        val FALSE: BDD = bddFactory.zero()


        private val featureIds: Map[String, Int] = Map()
        private val featureNames: Map[Int, String] = Map()
        private val featureBDDs: Map[Int, BDD] = Map()


        def and(a: FeatureExpr, b: FeatureExpr): FeatureExpr = new BDDFeatureExprImpl(a.bdd and b.bdd)
        def or(a: FeatureExpr, b: FeatureExpr): FeatureExpr = new BDDFeatureExprImpl(a.bdd or b.bdd)
        def imp(a: FeatureExpr, b: FeatureExpr): FeatureExpr = new BDDFeatureExprImpl(a.bdd imp b.bdd)
        def biimp(a: FeatureExpr, b: FeatureExpr): FeatureExpr = new BDDFeatureExprImpl(a.bdd biimp b.bdd)
        def xor(a: FeatureExpr, b: FeatureExpr): FeatureExpr = new BDDFeatureExprImpl(a.bdd xor b.bdd)

        def not(a: FeatureExpr): FeatureExpr = new BDDFeatureExprImpl(a.bdd.not())

        def definedExternal(name: String): FeatureExpr = {
            val id: Int = featureIds.get(name) match {
                case Some(id) => id
                case _ =>
                    maxFeatureId = maxFeatureId + 1
                    if (maxFeatureId >= bddVarNum) {
                        bddVarNum = bddVarNum * 2
                        bddFactory.setVarNum(bddVarNum)
                    }
                    featureIds.put(name, maxFeatureId)
                    featureNames.put(maxFeatureId, name)
                    featureBDDs.put(maxFeatureId, bddFactory.ithVar(maxFeatureId))
                    maxFeatureId
            }
            new BDDFeatureExprImpl(featureBDDs(id))
        }

        def lookupFeatureName(id: Int): String = featureNames(id)

        //create a macro definition (which expands to the current entry in the macro table; the current entry is stored in a closure-like way).
        //a form of caching provided by MacroTable, which we need to repeat here to create the same FeatureExpr object
        def definedMacro(name: String, macroTable: FeatureProvider): FeatureExpr = {
            macroTable.getMacroCondition(name).asInstanceOf[FeatureExpr]
        }
    }

}