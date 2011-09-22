package de.fosd.typechef.crewrite

import de.fosd.typechef.parser.c._
import de.fosd.typechef.conditional._
import de.fosd.typechef.featureexpr._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.Tag
import org.scalatest.matchers.ShouldMatchers

object simpletest extends Tag("simpletest")
object totest extends Tag("totest")

@RunWith(classOf[JUnitRunner])
class ControlFlowGraphTest extends FunSuite with TestHelper with ShouldMatchers with VariablesImpl with ControlFlowImpl {

  private def cp(pro: p.MultiParser[AST]) = pro ^^ { One(_) }

  private def parsePrintAST(code: String, pro: p.MultiParser[AST]) = {
    val ast = parse(code, cp(pro))
    println("AST: " + ast.get.asInstanceOf[One[AST]].value)
    println(PrettyPrinter.print(ast.get.asInstanceOf[One[AST]].value))
  }

  private def parsePrintASTGetAST(code: String, pro: p.MultiParser[AST]) = {
    val ast = parse(code, cp(pro)).get.asInstanceOf[One[AST]].value
    println("AST: " + ast)
    println(PrettyPrinter.print(ast))
    ast
  }

  private def parsePrintGetDefines(code: String, pro: p.MultiParser[AST]) = {
    val ast = parse(code, cp(pro))
    println("AST: " + ast.get)
    defines(ast.get.asInstanceOf[One[AST]].value)
  }

  private def parsePrintGetSucc(code: String, pro: p.MultiParser[AST]) = {
    val ast = parse(code, cp(pro))
    println("AST: " + ast.get)
    succ(ast.get.asInstanceOf[One[AST]].value)
  }

  test("forLoop") {
    parsePrintAST("""
    {
      for(;;) {
      }
    }
    """, p.compoundStatement)
  }

  test("nestedForLoop") {
    parsePrintAST("""
    {
      for(;;) {
        for(;;) {
          for(;;) {
          }
        }
      }
    }
    """, p.compoundStatement)
  }

  test("switchCase") {
    parsePrintAST("""
    {
      switch(x) {
      case 1: break;
      case 2: break;
      case 3: break;
      default: break;
      }
    }
    """, p.compoundStatement)
  }

  test("doWhileLoop") {
    parsePrintAST("""
    {
      do {
      } while (k);
    }
    """, p.compoundStatement)
  }

  test("simpleWhileLoop") {
    parsePrintAST("""
    {
      while (k) {
        int l;
        int m;
      }
    }
    """, p.compoundStatement)
  }

  test("ifthenelsechain") {
    parsePrintAST("""
    {
      int k = 3;
      if (k < 3) {
        k = -1;
      }
      #ifdef A
      else if (k = 3) {
        k = 0;
      }
      #endif
      else {
        k = 1;
      }
    }
    """, p.compoundStatement)
  }

//  test("conditionaldeclaration") {
//    parsePrintGetDefines("""
//    {
//      int k = 3;
//      #ifdef A
//      int l = 4;
//      #else
//      int m = 4;
//      #endif
//    }
//    """) should be(Set(Id("k"), Id("l"), Id("m")))
//  }
//
//  test("whileStatements") {
//    parsePrintGetDefines("""
//    {
//      while(k) { k -= 1; }
//      while(l) { l -= 1; }
//    }
//    """)
//  }
//
//  test("whilewrapper") {
//    parsePrintGetDefines("""
//    {
//      #ifdef A
//      while(k)
//      #endif
//      {
//        k -= 1;
//      }
//    }
//    """)
//  }
//
//  test("conditional labelstatements") {
//    parsePrintGetDefines("""
//    {
//      #if defined A
//        int a;
//      #elif defined B
//        int b;
//      #else
//        int c;
//      #endif
//      int d;
//    }
//    """)
//  }
//
//  test("conditional labelstatements pred elifelse") {
//    val e1 = Opt(True, LabelStatement(Id("e1"), None))
//    val e2 = Opt(fx, LabelStatement(Id("e2"), None))
//    val e3 = Opt(fy.and(fx.not), LabelStatement(Id("e3"), None))
//    val e4 = Opt(fy.not.and(fx.not), LabelStatement(Id("e4"), None))
//    val e5 = Opt(True, LabelStatement(Id("e5"), None))
//    val c = One(CompoundStatement(List(e1, e2, e3, e4, e5)))
//    println("AST: " + c)
//    println(PrettyPrinter.print(c.value))
//    e5->pred should be (Set(e2, e3, e4))
//    e4->pred should be (Set(e1))
//    e3->pred should be (Set(e1))
//    e2->pred should be (Set(e1))
//    e1->pred should be (Set(c.value))
//  }
//
  test("conditional labelstatements succ elifelse") {
    val e1 = Opt(True, LabelStatement(Id("e1"), None))
    val e2 = Opt(fx, LabelStatement(Id("e2"), None))
    val e3 = Opt(fy.and(fx.not), LabelStatement(Id("e3"), None))
    val e4 = Opt(fy.not.and(fx.not), LabelStatement(Id("e4"), None))
    val e5 = Opt(True, LabelStatement(Id("e5"), None))
    val c = One(CompoundStatement(List(e1, e2, e3, e4, e5)))
    println("AST: " + c)
    println(PrettyPrinter.print(c.value))
    succ(e1) should be (List(e2.entry, e3.entry, e4.entry))
    succ(e2) should be (List(e5.entry))
    succ(e3) should be (List(e5.entry))
    succ(e4) should be (List(e5.entry))
  }

  test("conditional labelstatements with sequence of annotated elements") {
    val e1 = Opt(True, LabelStatement(Id("e1"), None))
    val e2 = Opt(fx, LabelStatement(Id("e2"), None))
    val e3 = Opt(fx, LabelStatement(Id("e3"), None))
    val e4 = Opt(fx.not, LabelStatement(Id("e4"), None))
    val e5 = Opt(True, LabelStatement(Id("e5"), None))
    val c = One(CompoundStatement(List(e1, e2, e3, e4, e5)))
    succ(e1) should be(List(e2.entry, e4.entry))
    succ(e2) should be(List(e3.entry))
    succ(e3) should be(List(e5.entry))
    succ(e4) should be(List(e5.entry))
    DotGraph.map2file(getAllSucc(e1.entry))
  }

  test("conditional labelstatements with if and if-else blocks") {
    val e0 = Opt(fx, LabelStatement(Id("e0"), None))
    val e1 = Opt(True, LabelStatement(Id("e1"), None))
    val e2 = Opt(True, LabelStatement(Id("e2"), None))
    val e3 = Opt(fx, LabelStatement(Id("e3"), None))
    val e4 = Opt(fx.not.and(fy), LabelStatement(Id("e4"), None))
    val e5 = Opt(fx.not.and(fy.not), LabelStatement(Id("e5"), None))
    val e6 = Opt(fa, LabelStatement(Id("e6"), None))
    val e7 = Opt(fa.not, LabelStatement(Id("e7"), None))
    val e8 = Opt(fb.not, LabelStatement(Id("e8"), None))
    val e9 = Opt(True, LabelStatement(Id("e9"), None))
    val c = One(CompoundStatement(List(e0, e1, e2, e3, e4, e5, e6, e7, e8, e9)))
    succ(e0) should be(List(e1.entry))
    succ(e1) should be(List(e2.entry))
    succ(e2) should be(List(e3.entry, e4.entry, e5.entry))
    succ(e3) should be(List(e6.entry, e7.entry))
    succ(e4) should be(List(e6.entry, e7.entry))
    succ(e5) should be(List(e6.entry, e7.entry))
    succ(e6) should be(List(e8.entry, e9.entry))
    succ(e7) should be(List(e8.entry, e9.entry))
    succ(e8) should be(List(e9.entry))
    DotGraph.map2file(getAllSucc(e0.entry))
  }

  test("conditional declaration statement") {
    val e0 = Opt(True, LabelStatement(Id("e0"), None))
    val e1 = Opt(fx,
      DeclarationStatement(
        Declaration(
          List(Opt(True,IntSpecifier())),
          List(Opt(True,InitDeclaratorI(AtomicNamedDeclarator(List(),Id("k"),List()),List(),None))))))
    val e2 = Opt(fx.not,
      DeclarationStatement(
        Declaration(
          List(Opt(True,DoubleSpecifier())),
          List(Opt(True,InitDeclaratorI(AtomicNamedDeclarator(List(),Id("k"),List()),List(),None))))))
    val e3 = Opt(True, LabelStatement(Id("e3"), None))
    val c = One(CompoundStatement(List(e0, e1, e2, e3)))
    succ(e0) should be(List(e1.entry, e2.entry))
    succ(e1) should be(List(e3.entry))
    succ(e2) should be(List(e3.entry))
    DotGraph.map2file(getAllSucc(e0.entry))
  }

  test("conditional while statement") {
    val e0 = Opt(True, LabelStatement(Id("e0"), None))
    val e11 = Opt(True, LabelStatement(Id("e11"), None))
    val e12 = Opt(fy, LabelStatement(Id("e12"), None))
    val e1c = Id("k")
    val e1 = Opt(fx, WhileStatement(e1c, One(CompoundStatement(List(e11, e12)))))
    val e2 = Opt(True, LabelStatement(Id("e2"), None))
    val c = One(CompoundStatement(List(e0, e1, e2)))
    succ(e0) should be(List(e1.entry, e2.entry))
    succ(e1) should be(List(e1c))
    succ(e1c) should be(List(e11.entry, e2.entry))
    DotGraph.map2file(getAllSucc(e0.entry))
  }

  test("conditional for loop", totest) {
    val a = parsePrintASTGetAST("""
    {
      int k = 2;
      int i;
      for(i=0;
      #ifdef A
      i<10
      #endif
      ;i++) j++;
      int j;
    }
    """, p.compoundStatement)
    DotGraph.map2file(getAllSucc(childAST(a.children.next)))
  }

  test("conditional ifstatement") {
    val a = parsePrintASTGetAST("""
    {
      int k = 3;
      if (k < 2) { k = 2; }
      #ifdef A
      else if (k < 5) { k = 5; }
      #endif
      #ifdef B
      else if (k < 7) { k = 7; }
      #endif
      else { k = 10; }
      int l = 3;
    }
    """, p.compoundStatement)
    DotGraph.map2file(getAllSucc(childAST(a.children.next)))
  }

  test("conditional switch statement", totest) {
    val a = parsePrintASTGetAST("""
    {
      int k = 3;
      switch (k) {
      case 1: break;
      #ifdef A
      case 2: break;
      #endif
      case 3: break;
      default: break;
      }
      int l = 2;
    }
    """, p.compoundStatement)
    DotGraph.map2file(getAllSucc(childAST(a.children.next)))
  }

  test("conditional for loop elems") {
    val e0 = Opt(True, LabelStatement(Id("e0"), None))
    val e1 = Opt(fx, ForStatement(
      Some(AssignExpr(Id("i"),"=",Constant("0"))),
      Some(AssignExpr(Id("i"),"=",Constant("2"))),
      Some(PostfixExpr(Id("i"),SimplePostfixSuffix("++"))),
      One(CompoundStatement(List(Opt(fx,ExprStatement(PostfixExpr(Id("j"),SimplePostfixSuffix("++")))))))))
    val e2 = Opt(True, LabelStatement(Id("e2"), None))
    val c = One(CompoundStatement(List(e0, e1, e2)))
    DotGraph.map2file(getAllSucc(e0.entry))
  }

  test("conditional for loop alternative") {
    val a = parsePrintASTGetAST("""
    {
      int i;
      for(;;) {
      #ifdef A
      int a;
      #else
      double a;
      #endif
      }
      int j;
    }
    """, p.compoundStatement)
    DotGraph.map2file(getAllSucc(childAST(a.children.next)))
  }

  test("conditional for loop infinite") {
    val a = parsePrintASTGetAST("""
    {
      int i;
      for(;;) {
      }
      int j;
    }
    """, p.compoundStatement)
    DotGraph.map2file(getAllSucc(childAST(a.children.next)))
  }

  test("conditional for loop infinite single statement") {
    val a = parsePrintASTGetAST("""
    {
      int i = 0;
      for(;;) {
        i++;
      }
      int j;
    }
    """, p.compoundStatement)
    DotGraph.map2file(getAllSucc(childAST(a.children.next)))
  }

  test("conditional statements increment") {
    val a = parsePrintASTGetAST("""
    {
      int k = 0;
      k++;
      k++;
      k++;
    }
    """, p.compoundStatement)
    DotGraph.map2file(getAllSucc(childAST(a.children.next)))
  }

  test("conditional statements") {
    val a = parsePrintASTGetAST("""
    {
      int a = 2;
      int b = 200;
      while (
      #ifdef A
      a < b
      #else
      true
      #endif
      )
      {
        a++;
        #ifdef B
        b--;
        #endif
      }
      #ifdef C
      b = 20;
      a = 30;
      #endif
      while (a > b) {
        a++;
      }
      int c;
    }
    """, p.compoundStatement)
    DotGraph.map2file(getAllSucc(childAST(a.children.next)))
  }

  test("conditional label and goto statements", simpletest) {
    val a = parsePrintASTGetAST("""
    {
      goto label1;
      #ifdef A
      label1:
        int a;
      #else
      label1:
        int b;
      #endif
      label2:
    }
    """, p.compoundStatement)
    DotGraph.map2file(getAllSucc(childAST(a.children.next)))
  }


  test("conditional label and goto statements - constructed", totest) {
    val e0 = Opt(FeatureExpr.base, GotoStatement(Id("label1")))
    val e1 = Opt(fx, LabelStatement(Id("label1"), None))
    val e2 = Opt(fx, DeclarationStatement(Declaration(List(Opt(fx, IntSpecifier())), List(Opt(fx, InitDeclaratorI(AtomicNamedDeclarator(List(), Id("a"), List()), List(), None))))))
    val e3 = Opt(fx.not, LabelStatement(Id("label1"), None))
    val e4 = Opt(fx.not, DeclarationStatement(Declaration(List(Opt(fx.not, IntSpecifier())), List(Opt(fx.not, InitDeclaratorI(AtomicNamedDeclarator(List(), Id("b"), List()), List(), None))))))
    val e5 = Opt(FeatureExpr.base, LabelStatement(Id("label2"), None))
    val f = FunctionDef(List(Opt(FeatureExpr.base, VoidSpecifier())), AtomicNamedDeclarator(List(),Id("foo"),List(Opt(True,DeclIdentifierList(List())))), List(), CompoundStatement(List(e0, e1, e2, e3, e4, e5)))
    succ(e0) should be (List(e1.entry, e3.entry))
    succ(e1) should be (List(e2.entry))
    succ(e2) should be (List(e5.entry))
    succ(e3) should be (List(e4.entry))
    succ(e4) should be (List(e5.entry))
    DotGraph.map2file(getAllSucc(childAST(e0)))
  }

  test("testTChoice", totest) {
    val a = parsePrintASTGetAST("""
    {
      int a;
      #ifdef B
      int b;
      #elif defined(C)
      int c;
      #else
      int d;
      #endif
      int e;
    }
    """, p.compoundStatement)
    val t: TConditional[AST] = succ(childAST(a.children.next))
    print(t)
  }

//  test("boa hash.c") {
//    val a = parsePrintASTGetAST("""
//    {
//          int i;
//          hash_struct *temp;
//          int total = 0;
//          int count;
//
//          for (i = 0; i < MIME_HASHTABLE_SIZE; ++i) { /* these limits OK? */
//              if (mime_hashtable[i]) {
//                  count = 0;
//                  temp = mime_hashtable[i];
//                  while (temp) {
//                      temp = temp->next;
//                      ++count;
//                  }
//      #ifdef NOISY_SIGALRM
//                  log_error_time();
//                  fprintf(stderr, "mime_hashtable[%d] has %d entries\n",
//                          i, count);
//      #endif
//                  total += count;
//              }
//          }
//          log_error_time();
//          fprintf(stderr, "mime_hashtable has %d total entries\n",
//                  total);
//
//          total = 0;
//          for (i = 0; i < PASSWD_HASHTABLE_SIZE; ++i) { /* these limits OK? */
//              if (passwd_hashtable[i]) {
//                  temp = passwd_hashtable[i];
//                  count = 0;
//                  while (temp) {
//                      temp = temp->next;
//                      ++count;
//                  }
//      #ifdef NOISY_SIGALRM
//                  log_error_time();
//                  fprintf(stderr, "passwd_hashtable[%d] has %d entries\n",
//                          i, count);
//      #endif
//                  total += count;
//              }
//          }
//
//          log_error_time();
//          fprintf(stderr, "passwd_hashtable has %d total entries\n",
//                  total);
//
//      }
//
//    """)
//    DotGraph.map2file(getAllSucc(childAST(a.children.next)))
//  }
}