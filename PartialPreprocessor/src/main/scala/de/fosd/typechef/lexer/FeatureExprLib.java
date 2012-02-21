package de.fosd.typechef.lexer;

import de.fosd.typechef.featureexprJava.*;
import de.fosd.typechef.featureexprInterface.*;
import de.fosd.typechef.featureexprUtil.*;

/**
 * helper class for more conventient access to scala library
 *
 * @author kaestner
 */
public class FeatureExprLib {
    //public static interface FeatureExpr extends AbstractFeatureExprModule.AbstractFeatureExpr {}
    //interface FeatureExpr extends foo.AbstractFeatureExpr {}
    public static AbstractFeatureExprModule.AbstractFeatureExprFactory l() {
	return FeatureExprLibScala.l();
    }

    public static AbstractFeatureExprModule.AbstractFeatureExpr base() {
	return FeatureExprLibScala.base();
    }

    public static AbstractFeatureExprModule.AbstractFeatureExpr dead() {
	return FeatureExprLibScala.dead();
    }

    //TODO: for some reason FeatureExprTree<Long> does no longer work since
    //scala 2.9. We use FeatureExprTree<Object>, which seems unproblematic since
    //the result is not constructed or inspected in Java code anyway, but only passed around
    public static FeatureExprTree<Object> zero() {
	return FeatureExprLibScala.zero();
    }

    public static AbstractFeatureExprModule.AbstractFeatureExpr toFeatureExpr(FeatureExprTree<Object> v) {
	return FeatureExprLibScala.toFeatureExpr(v);
    }
}

////import de.fosd.typechef.featureexprJava.FeatureExpr;
///*
//import de.fosd.typechef.featureexpr.FeatureExpr$;
//import de.fosd.typechef.featureexpr.FeatureExprValue$;
//*/
//import de.fosd.typechef.featureexpr.package$;
//import de.fosd.typechef.featureexprUtil.*;
//import de.fosd.typechef.featureexprUtil.FeatureExprTree;
//import de.fosd.typechef.featureexprUtil.FeatureExprValue$;
//
//import de.fosd.typechef.featureexprInterface.AbstractFeatureExprModule;
//

//public class FeatureExprLib {
//    Object foo = de.fosd.typechef.package$.MODULE$;
//
//    //interface FeatureExpr extends foo.AbstractFeatureExpr {}
//    public static FeatureExpr$ l() {
//	new foo.AbstractFeatureExpr();
//        return FeatureExpr$.MODULE$;
//    }
//
//    public static FeatureExpr base() {
//        return FeatureExpr$.MODULE$.base();
//    }
//
//    public static FeatureExpr dead() {
//        return FeatureExpr$.MODULE$.dead();
//    }
//
//    //TODO: for some reason FeatureExprTree<Long> does no longer work since
//    //scala 2.9. We use FeatureExprTree<Object>, which seems unproblematic since
//    //the result is not constructed or inspected in Java code anyway, but only passed around
//    public static de.fosd.typechef.featureexprUtil.FeatureExprTree<Object> zero() {
//        return FeatureExpr$.MODULE$.createInteger(0);
//    }
//
//    public static FeatureExpr toFeatureExpr(FeatureExprTree<Object> v) {
//        return FeatureExprValue$.MODULE$.toFeatureExpr(v);
//    }
//}

