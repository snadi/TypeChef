package de.fosd.typechef.crewrite

import de.fosd.typechef.parser.c._
import de.fosd.typechef.conditional._
import junit.framework.TestCase

class ControlFlowGraphTest extends TestCase with TestHelper with VariablesImpl {

  private def cp(pro: p.MultiParser[AST]) = pro ^^ { One(_) }

  private def parsePrintGetDefines(code: String) = {
    val ast = parse(code, cp(p.compoundStatement))
    println("AST: " + ast.get)
    println("defines: " + defines(ast.get.asInstanceOf[One[AST]].value))
  }

  def testStatement = {
    parsePrintGetDefines("""
    {
      int k = 2;
      int l = 3;
    }
    """)
    parsePrintGetDefines("""
    {
      int k,l = 3;
      k = 4;
    }
    """)
    parsePrintGetDefines("""
    {
      int k = 3;
      while (k) {
        int l = 2;
      }
    }
    """)
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

}