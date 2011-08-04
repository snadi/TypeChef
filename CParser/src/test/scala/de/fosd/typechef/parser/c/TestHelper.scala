package de.fosd.typechef.parser.c

import de.fosd.typechef.featureexpr.FeatureExpr
import java.io.InputStream
import scala.io.Source
import de.fosd.typechef.parser.c.PrettyPrinter._
import de.fosd.typechef.conditional.{One, Conditional}
import de.fosd.typechef.parser.TokenReader
import org.junit.Assert._

/**
 * common infrastructure for tests.
 * mainly for parsing
 */

trait TestHelper extends EnforceTreeHelper {

    val fa = FeatureExpr.createDefinedExternal("A")
    val fb = FeatureExpr.createDefinedExternal("B")
    val fc = FeatureExpr.createDefinedExternal("C")
    val fx = FeatureExpr.createDefinedExternal("X")
    val fy = FeatureExpr.createDefinedExternal("Y")

    def getAST(code: String): TranslationUnit = {
        val ast: AST = new ParserMain(new CParser).parserMain(
            () => CLexer.lex(code, null), new CTypeContext, false)
        prepareAST(ast)
    }

    def parseFile(stream: InputStream, file: String, dir: String): TranslationUnit = {
        val ast: AST = new ParserMain(new CParser).parserMain(
            () => CLexer.lexStream(stream, file, dir, null), new CTypeContext, false)
        println("preparing AST...")
        prepareAST(ast)
    }

    def getResult(file: String, dir: String) = Source.fromFile(dir + file).mkString

    def parseExpr(code: String): Expr = {
        val in = CLexer.lex(code, null).setContext(new CTypeContext())
        val p = new CParser()
        val r = p.phrase(p.expr)(in, FeatureExpr.base)
        r.asInstanceOf[p.Success[Expr]].result
    }

    def parseDecl(code: String): Declaration = {
        val in = CLexer.lex(code, null).setContext(new CTypeContext())
        val p = new CParser()
        val r = p.phrase(p.declaration)(in, FeatureExpr.base)
        r.asInstanceOf[p.Success[Declaration]].result
    }

    val p = new CParser()

  def parsePrintParse(code: String, production: p.MultiParser[AST]) {
    parsePrintParseCond(code, production ^^ {
      One(_)
    })
  }

  private def parsePrintParseCond(code: String, production: p.MultiParser[Conditional[AST]]) {

    //parse
    val ast = parse(code, production)

    println("AST: " + ast.get)


    //pretty print
    val doc = prettyPrint(ast.get.asInstanceOf[One[AST]].value) //temporary workaround with typecast
    val printed = layout(doc)

    println("Pretty: " + printed)

    val ast2 = parse(printed, production)
    println("new AST: " + ast2.get)

    assertEquals("AST after parsing printed result is different\n" + printed, ast.get, ast2.get)
  }

  def parse[T](code: String, production: (TokenReader[TokenWrapper, CTypeContext], FeatureExpr) => p.MultiParseResult[T]): Option[T] = {
    val actual = p.parse(code.stripMargin, production)
    (actual: @unchecked) match {
      case p.Success(ast, unparsed) => {
        assertTrue("parser did not reach end of token stream: " + unparsed, unparsed.atEnd)
        Some(ast)
      }
      case p.NoSuccess(msg, unparsed, inner) =>
        fail(msg + " at " + unparsed + " " + inner + "\nin " + code)
        None
    }
  }

}