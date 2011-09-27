package de.fosd.typechef.crewrite

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Tag

import de.fosd.typechef.parser.c._
import de.fosd.typechef.conditional._

@RunWith(classOf[JUnitRunner])
class CAnalysisTest extends FunSuite with TestHelper with ShouldMatchers with CAnalysis{

  object simpletest extends Tag("simpletest")
  object totest extends Tag("totest")

  private def cp(pro: p.MultiParser[AST]) = pro ^^ {One(_)}

  private def parsePrintCC(code: String, pro: p.MultiParser[AST]) = {
    val ast = parse(code, cp(pro)).get.asInstanceOf[One[AST]].value
    println(ast)
    println(PrettyPrinter.print(ast))
    println("cyclomatic complexity: " + cc(ast))
  }

  private def parsePrintDeadCode(code: String) = {
    val ast = parse(code, cp(p.functionDef)).get.asInstanceOf[One[AST]].value
    println(ast)
    println(PrettyPrinter.print(ast))
    println("dead code: " + deadCode(ast.asInstanceOf[FunctionDef]))
  }

  test("if-then-else", totest) {
    parsePrintCC("""
    {
      #ifdef A
      int a;
      #else
      int b;
      #endif
    }
    """, p.compoundStatement)
  }

  test("if-then", totest) {
    parsePrintCC("""
    {
      int a;
      #ifdef B
      int b;
      #endif
    }
    """, p.compoundStatement)
  }

  test("switch-case", totest) {
    parsePrintCC("""
    {
      switch (x) {
       case 1:
       case 2:
       case 3: break;
      }
    }
    """, p.compoundStatement)
  }

  test("if-then with &&", simpletest) {
    parsePrintCC("""
    {
      if (a && b || c) {
        int k;
      }
      #if defined(A) && defined(B)
      int l;
      #endif
    }
    """, p.compoundStatement)
  }

  test("simple deadcode return", totest) {
    parsePrintDeadCode("""
    void foo() {
      int a;
      return a;
      int b;
    }
    """)
  }

  test("simple deadcode return variable", totest) {
    parsePrintDeadCode("""
    void foo() {
      int a;
      #ifdef A
      return a;
      #endif
      int b;
    }
    """)
  }
}