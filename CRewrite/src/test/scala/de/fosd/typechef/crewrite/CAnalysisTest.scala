package de.fosd.typechef.crewrite

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.Tag
import org.scalatest.matchers.ShouldMatchers

import de.fosd.typechef.parser.c._
import de.fosd.typechef.conditional._

object totest extends Tag("totest")

@RunWith(classOf[JUnitRunner])
class CAnalysisTest extends FunSuite with TestHelper with ShouldMatchers with CAnalysis{
  private def cp(pro: p.MultiParser[AST]) = pro ^^ {One(_)}

  private def parsePrintCC(code: String) = {
    val ast = parse(code, cp(p.compoundStatement)).get.asInstanceOf[One[AST]].value
    println(ast)
    println("cyclomatic complexity: " + cc(ast))
  }

  test("if-then-else", totest) {
    parsePrintCC("""
    {
      #ifdef A
      int a;
      #elif defined(B)
      int b;
      #else
      int c;
      #endif
    }
    """)
  }

  test("if-then", totest) {
    parsePrintCC("""
    {
      int a;
      #ifdef B
      int b;
      #endif
    }
    """)
  }

}