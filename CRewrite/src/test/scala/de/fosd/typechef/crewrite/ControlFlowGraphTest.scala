package de.fosd.typechef.crewrite

import de.fosd.typechef.parser.c._
import de.fosd.typechef.conditional._
import de.fosd.typechef.featureexpr._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.Tag
import org.scalatest.matchers.ShouldMatchers

object totest extends Tag("totest")

@RunWith(classOf[JUnitRunner])
class ControlFlowGraphTest extends FunSuite with TestHelper with ShouldMatchers with VariablesImpl with ControlFlowImpl {

  private def cp(pro: p.MultiParser[AST]) = pro ^^ { One(_) }

  private def parsePrintAST(code: String) = {
    val ast = parse(code, cp(p.compoundStatement))
    println("AST: " + ast.get.asInstanceOf[One[AST]].value)
    println(PrettyPrinter.print(ast.get.asInstanceOf[One[AST]].value))
  }

  private def parsePrintASTGetAST(code: String) = {
    val ast = parse(code, cp(p.compoundStatement)).get.asInstanceOf[One[AST]].value
    println("AST: " + ast)
    println(PrettyPrinter.print(ast))
    ast
  }

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

//  private def parsePrintGetPred(code: String) = {
//    val ast = parse(code, cp(p.compoundStatement))
//    println("AST: " + ast.get)
//    pred(ast.get.asInstanceOf[One[AST]].value)
//  }

//  test("twoDeclarations") {
//    parsePrintGetDefines("""
//    {
//      int k = 2;
//      int l = 3;
//    }
//    """) should be(Set(Id("k"), Id("l")))
//  }
//
//  test("multipleDeclarations") {
//    parsePrintGetDefines("""
//    {
//      int k,l = 3;
//      k = 4;
//    }
//    """) should be(Set(Id("k"), Id("l")))
//  }


  test("forLoop") {
    parsePrintAST("""
    {
      for(;;) {
      }
    }
    """)
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
    """)
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
    """)
  }

  test("doWhileLoop") {
    parsePrintAST("""
    {
      do {
      } while (k);
    }
    """)
  }

  test("simpleWhileLoop") {
    parsePrintAST("""
    {
      while (k) {
        int l;
        int m;
      }
    }
    """)
  }

  test("ifthenelsechain") {
    parsePrintAST("""
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
//  test("conditional labelstatements succ elifelse") {
//    val e1 = Opt(True, LabelStatement(Id("e1"), None))
//    val e2 = Opt(fx, LabelStatement(Id("e2"), None))
//    val e3 = Opt(fy.and(fx.not), LabelStatement(Id("e3"), None))
//    val e4 = Opt(fy.not.and(fx.not), LabelStatement(Id("e4"), None))
//    val e5 = Opt(True, LabelStatement(Id("e5"), None))
//    val c = One(CompoundStatement(List(e1, e2, e3, e4, e5)))
//    println("AST: " + c)
//    println(PrettyPrinter.print(c.value))
//    e1->succ should be (Set(e2, e3, e4))
//    e2->succ should be (Set(e5))
//    e3->succ should be (Set(e5))
//    e4->succ should be (Set(e5))
//    e5->succ should be (Set(null))
//  }
//
//  test("conditional labelstatements pred if") {
//    val e1 = Opt(True, LabelStatement(Id("e1"), None))
//    val e2 = Opt(fx, LabelStatement(Id("e2"), None))
//    val e3 = Opt(True, LabelStatement(Id("e3"), None))
//    val c = One(CompoundStatement(List(e1, e2, e3)))
//    println("AST: " + c)
//    println(PrettyPrinter.print(c.value))
//    e1->pred should be (Set(c.value))
//    e2->pred should be (Set(e1))
//    e3->pred should be (Set(e1))
//  }
//
//  test("conditional labelstatements isPartOfIEEChain") {
//    val e1 = Opt(True, LabelStatement(Id("e1"), None))
//    val e2 = Opt(fx, LabelStatement(Id("e2"), None))
//    val e3 = Opt(fx.not, LabelStatement(Id("e3"), None))
//    val e4 = Opt(True, LabelStatement(Id("e4"), None))
//    val e5 = Opt(True, LabelStatement(Id("e5"), None))
//    val e6 = Opt(fx, LabelStatement(Id("e6"), None))
//    val e7 = Opt(True, LabelStatement(Id("e7"), None))
//    val c = One(CompoundStatement(List(e1, e2, e3, e4, e5, e6, e7)))
////    println("AST: " + c)
////    println(PrettyPrinter.print(c.value))
//
//  }
//
//  test("conditional labelstatements isPartOfIEEChain2") {
//    val e1 = Opt(True, LabelStatement(Id("e1"), None))
//    val e2 = Opt(fx, LabelStatement(Id("e2"), None))
//    val e3 = Opt(fx.not, LabelStatement(Id("e3"), None))
//    val e4 = Opt(True, LabelStatement(Id("e4"), None))
//    val e5 = Opt(fx, LabelStatement(Id("e5"), None))
//    val e6 = Opt(fy, LabelStatement(Id("e6"), None))
//    val e7 = Opt(fy.not, LabelStatement(Id("e7"), None))
//    val c = One(CompoundStatement(List(e1, e2, e3, e4, e5, e6, e7)))
////    println("AST: " + c)
////    println(PrettyPrinter.print(c.value))
//
//  }

  test("conditional labelstatements with sequence of annotated elements") {
    val e1 = Opt(True, LabelStatement(Id("e1"), None))
    val e2 = Opt(fx, LabelStatement(Id("e2"), None))
    val e3 = Opt(fx, LabelStatement(Id("e3"), None))
    val e4 = Opt(fx.not, LabelStatement(Id("e4"), None))
    val e5 = Opt(True, LabelStatement(Id("e5"), None))
    val c = One(CompoundStatement(List(e1, e2, e3, e4, e5)))
    succ(e1) should be(Set(e2.entry, e4.entry))
    succ(e2) should be(Set(e3.entry))
    succ(e3) should be(Set(e5.entry))
    succ(e4) should be(Set(e5.entry))
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
    succ(e0) should be(Set(e1.entry))
    succ(e1) should be(Set(e2.entry))
    succ(e2) should be(Set(e3.entry, e4.entry, e5.entry))
    succ(e3) should be(Set(e6.entry, e7.entry))
    succ(e4) should be(Set(e6.entry, e7.entry))
    succ(e5) should be(Set(e6.entry, e7.entry))
    succ(e6) should be(Set(e8.entry, e9.entry))
    succ(e7) should be(Set(e8.entry, e9.entry))
    succ(e8) should be(Set(e9.entry))
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
    succ(e0) should be(Set(e1.entry, e2.entry))
    succ(e1) should be(Set(e3.entry))
    succ(e2) should be(Set(e3.entry))
    DotGraph.map2file(getAllSucc(e0.entry))
  }

  test("conditional while statement") {
    val e0 = Opt(True, LabelStatement(Id("e0"), None))
    val e11 = Opt(True, LabelStatement(Id("e11"), None))
    val e12 = Opt(fy, LabelStatement(Id("e12"), None))
    val e1 = Opt(fx, WhileStatement(Id("k"), One(
      CompoundStatement(List(e11, e12)))))
    val e2 = Opt(True, LabelStatement(Id("e2"), None))
    val c = One(CompoundStatement(List(e0, e1, e2)))
    succ(e0) should be(List(e1.entry, e2.entry))
    succ(e1) should be(List(e2.entry, e11.entry))
    DotGraph.map2file(getAllSucc(e0.entry))
  }

  test("conditional for loop") {
    val a = parsePrintASTGetAST("""
    {
      int k = 2;
      int i;
      for(i=0;
      #ifdef A
      i<10
      #endif
      ;i++) { j++; }
      int j;
    }
    """)
    DotGraph.map2file(getAllSucc(childAST(a.children.next)))
  }

  test("conditional for loop elems", totest) {
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

  test("conditional for loop alternative", totest) {
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
    """)
    DotGraph.map2file(getAllSucc(childAST(a.children.next)))
  }

  test("conditional for loop infinite", totest) {
    val a = parsePrintASTGetAST("""
    {
      int i;
      for(;;) {
      }
      int j;
    }
    """)
    DotGraph.map2file(getAllSucc(childAST(a.children.next)))
  }

  test("conditional for loop infinite single statement", totest) {
    val a = parsePrintASTGetAST("""
    {
      int i = 0;
      for(;;) {
        i++;
      }
      int j;
    }
    """)
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
    """)
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
    """)
    DotGraph.map2file(getAllSucc(childAST(a.children.next)))
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