package de.fosd.typechef.crewrite

import de.fosd.typechef.parser.c._
import de.fosd.typechef.conditional._
import junit.framework.TestCase

class ControlFlowGraphTest extends TestCase with TestHelper with VariablesImpl {

  private def cp(pro: p.MultiParser[AST]) = pro ^^ { One(_) }

  def testStatement = {
    val ast = parse("""{int k = 2; }""", cp(p.compoundStatement))
    println("AST: " + ast.get)
    println("defines: " + defines(ast.get.asInstanceOf[One[AST]].value))
  }

}