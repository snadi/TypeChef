package de.fosd.typechef.crewrite

import de.fosd.typechef.parser.c._
import de.fosd.typechef.conditional._
import de.fosd.typechef.featureexpr._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.Tag
import org.scalatest.matchers.ShouldMatchers

@RunWith(classOf[JUnitRunner])
class ControlFlowGraphTest extends FunSuite with TestHelper with ShouldMatchers with ControlFlowImpl {

  object totest extends Tag("totest")

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

  private def parsePrintGetSucc(code: String, pro: p.MultiParser[AST]) = {
    val ast = parse(code, cp(pro))
    println("AST: " + ast.get)
    succ(ast.get.asInstanceOf[One[AST]].value)
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

  test("conditional switch statement") {
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

  test("conditional label and jump statements") {
    val a = parsePrintASTGetAST("""
    {
      label1:
      int k;
      int l;
      if (l != 0)
        goto label1;
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

  test("conditional label and goto statements") {
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

  test("cfg function") {
    val a = parsePrintASTGetAST("""
    void foo() {
      #ifdef A
      int a;
      #else
      int anot;
      #endif
    }
    """, p.functionDef)
    DotGraph.map2file(getAllSucc(a))
  }

  test("variable return statement", totest) {
    val a = parsePrintASTGetAST("""
      void foo() {
        int a;
        #ifdef R
        if (a) return a;
        #endif
        int b;
      }
    """, p.functionDef)
    DotGraph.map2file(getAllSucc(a))
  }

  test("boa hash.c") {
    val a = parsePrintASTGetAST("""
    {
          int i;
          hash_struct *temp;
          int total = 0;
          int count;

          for (i = 0; i < MIME_HASHTABLE_SIZE; ++i) { /* these limits OK? */
              if (mime_hashtable[i]) {
                  count = 0;
                  temp = mime_hashtable[i];
                  while (temp) {
                      temp = temp->next;
                      ++count;
                  }
      #ifdef NOISY_SIGALRM
                  log_error_time();
                  fprintf(stderr, "mime_hashtable[%d] has %d entries\n",
                          i, count);
      #endif
                  total += count;
              }
          }
          log_error_time();
          fprintf(stderr, "mime_hashtable has %d total entries\n",
                  total);

          total = 0;
          for (i = 0; i < PASSWD_HASHTABLE_SIZE; ++i) { /* these limits OK? */
              if (passwd_hashtable[i]) {
                  temp = passwd_hashtable[i];
                  count = 0;
                  while (temp) {
                      temp = temp->next;
                      ++count;
                  }
      #ifdef NOISY_SIGALRM
                  log_error_time();
                  fprintf(stderr, "passwd_hashtable[%d] has %d entries\n",
                          i, count);
      #endif
                  total += count;
              }
          }

          log_error_time();
          fprintf(stderr, "passwd_hashtable has %d total entries\n",
                  total);

      }

    """, p.compoundStatement)
    DotGraph.map2file(getAllSucc(childAST(a.children.next)))
  }
}