package de.fosd.typechef.crewrite

import de.fosd.typechef.parser.c._
import de.fosd.typechef.conditional._
import de.fosd.typechef.featureexpr._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

@RunWith(classOf[JUnitRunner])
class ControlFlowGraphTest extends FunSuite with TestHelper with ShouldMatchers with VariablesImpl with ControlFlowImpl {

  private def cp(pro: p.MultiParser[AST]) = pro ^^ { One(_) }

  private def parsePrintGetDefines(code: String) = {
    val ast = parse(code, cp(p.compoundStatement))
    println("AST: " + ast.get)
    defines(ast.get.asInstanceOf[One[AST]].value)
  }

  private def parsePrintGetSucc(code: String) = {
    val ast = parse(code, cp(p.compoundStatement))
    println("AST: " + ast.get)
    succ(ast.get.asInstanceOf[One[AST]].value)
  }

  test("twoDeclarations") {
    parsePrintGetDefines("""
    {
      int k = 2;
      int l = 3;
    }
    """) should be(Set(Id("k"), Id("l")))
  }

  test("multipleDeclarations") {
    parsePrintGetDefines("""
    {
      int k,l = 3;
      k = 4;
    }
    """) should be(Set(Id("k"), Id("l")))
  }

  test("whileLoop") {
    parsePrintGetDefines("""
    {
      int k = 3;
      while (k) {
        int l = 2;
      }
    }
    """) should be(Set(Id("k"), Id("l")))
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
    """) should be(Set(Id("k")))
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
    """) should be(Set(Id("k"), Id("l"), Id("m")))
  }

  test("whileStatements") {
    parsePrintGetDefines("""
    {
      while(k) { k -= 1; }
      while(l) { l -= 1; }
    }
    """)
  }

  test("whilewrapper") {
    parsePrintGetDefines("""
    {
      #ifdef A
      while(k)
      #endif
      {
        k -= 1;
      }
    }
    """)
  }

  test("emptystatement") {
    parsePrintGetSucc("""
    {
      ;
    }
    """) should be(Set(Opt(FeatureExpr.base, EmptyStatement())))
  }

  test("conditional labelstatements") {
    parsePrintGetDefines("""
    {
      #if defined A
        int a;
      #elif defined B
        int b;
      #else
        int c;
      #endif
      int d;
    }
    """)
  }


  test("labelstatements") {
    val e1 = Opt(True, LabelStatement(Id("e1"), None))
    val e2 = Opt(fx, LabelStatement(Id("e2"), None))
    val e3 = Opt(fx, LabelStatement(Id("e3"), None))
    val e4 = Opt(True, LabelStatement(Id("e4"), None))
    val c = One(CompoundStatement(List(e1, e2, e3, e4)))
    println("AST: " + c)
    e1->succ should be (Set(e2, e4))
    e1->succ should not be (None)
    e2->succ should be (Set(e3))
    e3->succ should be (Set(e4))
  }
}