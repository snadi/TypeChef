package de.fosd.typechef.typesystem

import de.fosd.typechef.parser.c._
import de.fosd.typechef.featureexpr._
import de.fosd.typechef.conditional._
import linker.CInferInterface
import de.fosd.typechef.error._

/**
 * checks an AST (from CParser) for type errors (especially dangling references)
 *
 * performs type checking in a single tree-walk, uses lookup functions from various traits
 *
 * @author kaestner
 *
 */

class CTypeSystemFrontend(iast: TranslationUnit,
                          featureModel: FeatureModel = FeatureExprFactory.default.featureModelFactory.empty,
                          options: ICTypeSysOptions = LinuxDefaultOptions) extends CTypeSystem with CInferInterface {

    //overwrites the default options
    override protected def opts: ICTypeSysOptions = options

    def prettyPrintType(ctype: Conditional[CType]): String =
        Conditional.toOptList(ctype).map(o => o.feature.toString + ": \t" + o.entry).mkString("\n")

    private def indentAllLines(s: String): String =
        s.lines.map("\t\t" + _).foldLeft("")(_ + "\n" + _)

    var errors: List[TypeChefError] = List()


    val DEBUG_PRINT = false

    def dbgPrint(o: Any) = if (DEBUG_PRINT) print(o)

    def dbgPrintln(o: Any) = if (DEBUG_PRINT) println(o)

    val verbose = false


    var externalDefCounter: Int = 0
    override def checkingExternal(externalDef: ExternalDef) = {
        externalDefCounter = externalDefCounter + 1
        if (verbose)
            println("check " + externalDefCounter + "/" + iast.defs.size + ". line " + externalDef.getPositionFrom.getLine + ". err " + errors.size)
    }
    override def issueTypeError(severity: Severity.Severity, condition: FeatureExpr, msg: String, where: AST, severityExtra: String = "") =
    //first check without feature model for performance reasons
        if (condition.isSatisfiable() && condition.isSatisfiable(featureModel)) {
            val e = new TypeChefError(severity, condition, msg, where, severityExtra)
            errors = e :: errors
            println("  - " + e)
        }


    /**
     * Returns true iff no errors were found.
     * @return
     */
    def checkAST(ignoreWarnings: Boolean = true): Boolean = {

        errors = List() // clear error list
        typecheckTranslationUnit(iast)
        val merrors = if (ignoreWarnings)
            errors.filterNot(Set(Severity.Warning, Severity.SecurityWarning) contains _.severity)
        else errors
        if (merrors.isEmpty)
            println("No type errors found.")
        else {
            println("Found " + merrors.size + " type errors: ");
        }
        //println("\n")
        return merrors.isEmpty
    }
    def checkASTSilent: Boolean = {
        errors = List() // clear error list
        typecheckTranslationUnit(iast)
        return errors.isEmpty
    }
}


