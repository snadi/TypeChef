package de.fosd.typechef.typesystem.statistics

import de.fosd.typechef.typesystem.linker.InterfaceWriter
import de.fosd.typechef.conditional.Conditional
import de.fosd.typechef.typesystem.{CType, CTypeSystem}
import de.fosd.typechef.featureexpr.FeatureExpr
import de.fosd.typechef.parser.c.{Id, Expr}
import de.fosd.typechef.typesystem.linker.CSignature._


trait CStatistics extends CTypeSystem with InterfaceWriter {


    /**
     * all function declarations without definitions are imports
     * if they are referenced at least once
     */
    override def typedExpr(expr: Expr, ctypes: Conditional[CType], featureExpr: FeatureExpr) {
        expr match {
            case identifier: Id =>
                for ((fexpr, ctype) <- ctypes.toList)
                    if (fexpr and (featureExpr) isSatisfiable) {
                        //kind; id; source condition; target condition; type
                        val kind = if (ctype.isFunction) "function call" else "variable access"
                        println(kind + ";" + identifier.name + ";" + featureExpr + ";" + fexpr + ";" + ctype.toValue.toText)
                    }
            case _ =>
        }


    }

}
