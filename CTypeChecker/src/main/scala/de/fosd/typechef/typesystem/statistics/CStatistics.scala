package de.fosd.typechef.typesystem.statistics

import de.fosd.typechef.typesystem.linker.InterfaceWriter
import de.fosd.typechef.conditional.Conditional
import de.fosd.typechef.typesystem.{CType, CTypeSystem}
import de.fosd.typechef.featureexpr.FeatureExpr
import de.fosd.typechef.typesystem.linker.CSignature._
import de.fosd.typechef.parser.c.{PointerPostfixSuffix, PostfixExpr, Id, Expr}


trait CStatistics extends CTypeSystem with InterfaceWriter {


    /**
     * all function declarations without definitions are imports
     * if they are referenced at least once
     */
    override def typedExpr(expr: Expr, ctypes: Conditional[CType], sourceFExpr: FeatureExpr) {
        for ((targetFExpr, ctype) <- ctypes.toList)
            if (targetFExpr and (sourceFExpr) isSatisfiable)
                expr match {
                    case identifier: Id =>
                        //kind; id; source condition; target condition; type
                        val kind = if (ctype.isFunction) "function call" else "variable access"
                        println(kind + ";" + identifier.name + ";" + sourceFExpr + ";" + targetFExpr + ";" + ctype.toValue.toText)
//                    case p@PostfixExpr(expr, PointerPostfixSuffix(".", i@Id(id))) =>


                    case _ =>
                }


    }

}
