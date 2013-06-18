package de.fosd.typechef.lexer.options

import io.Source
import de.fosd.typechef.featureexpr.FeatureExprFactory._

object PartialConfigurationParser {
    def load(file: String): PartialConfiguration = {
        val DEF = "#define"
        val UNDEF = "#undef"

        val directives = Source.fromFile(file).getLines().filter(_.startsWith("#")).toList

        def findMacroName(directive: String) = directive.split(' ')(1)

        val booleanDefs = directives.filter(_.startsWith(DEF)).map(findMacroName)
        val undefs = directives.filter(_.startsWith(UNDEF)).map(findMacroName)

        val featureExpr =
            (booleanDefs.map(createDefinedExternal(_)) ++
                undefs.map(createDefinedExternal(_).not())).
                foldRight(True)(_ and _)

        new PartialConfiguration(booleanDefs.toArray, undefs.toArray, featureExpr)
    }
}

