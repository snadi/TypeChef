package de.fosd.typechef.crewrite

import de.fosd.typechef.parser.c._
import de.fosd.typechef.conditional._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite

@RunWith(classOf[JUnitRunner])
class RefactoringTest extends FunSuite with TestHelper with VariablesImpl {

  private def cp(pro: p.MultiParser[AST]) = pro ^^ { One(_) }

  private def parsePrintGetDefines(code: String) = {
    val ast = parse(code, cp(p.compoundStatement))
    println("AST: " + ast.get)
    println("defines: " + defines(ast.get.asInstanceOf[One[AST]].value))
  }

  test("twoDeclarations") {
    parsePrintGetDefines("""
    {
      int k = 2;
      int l = 3;
    }
    """)
  }

  test("multipleDeclarations") {
    parsePrintGetDefines("""
    {
      int k,l = 3;
      k = 4;
    }
    """)
  }

  test("whileLoop") {
    parsePrintGetDefines("""
    {
      int k = 3;
      while (k) {
        int l = 2;
      }
    }
    """)
  }

  test("ifthenelsechain") {
    parsePrintGetDefines("""
    {
      int k = 3;
      if (k < 3) {
        k = -1;
      }
      else if (k = 3) {
        k = 0;
      }
      else {
        k = 1;
      }
    }
    """)
  }

  test("conditionaldeclaration") {
    parsePrintGetDefines("""
    {
      int k = 3;
      #ifdef A
      int l = 4;
      #else
      int m = 4;
      #endif
    }
    """)
  }
}