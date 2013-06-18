package de.fosd.typechef.crewrite

import org.junit.Test
import de.fosd.typechef.parser.c.{FunctionDef, PrettyPrinter, TestHelper}
import de.fosd.typechef.featureexpr.FeatureExprFactory

class SignAnalysisTest extends TestHelper with SignAnalysis with ConditionalControlFlow with Liveness {

    @Test def test_simple() {
        val a = parseFunctionDef( """
     void m() {
        int x = 0;
        #ifdef A
        x++;
        #endif
        #ifdef B
        x--;
        #endif
     }
                                  """)

        val env = CASTEnv.createASTEnv(a)
        setEnv(env)
        println("succs: " + DotGraph.map2file(getAllSucc(a, FeatureExprFactory.empty, env), env))
        println("preds: " + DotGraph.map2file(getAllPred(a, FeatureExprFactory.empty, env), env))

        val ss = getAllSucc(a.stmt.innerStatements.head.entry, FeatureExprFactory.empty, env).map(_._1).filterNot(_.isInstanceOf[FunctionDef])

        println("#################################################")
        val udr = determineUseDeclareRelation(a)
        setUdr(udr)
        setFm(FeatureExprFactory.empty)

        for (s <- ss)
            println(PrettyPrinter.print(s) + "  out: " + out(s) +
                "   in: " + in(s))
    }
}
