package de.fosd.typechef.parser.c

import de.fosd.typechef.conditional._
import org.kiama.attribution.Attribution._
import org.kiama._
import attribution.Attributable

/**
 * Simplified navigation support
 *
 * prevAST, nextAST, and parentAST provide navigation between
 * AST nodes not affected by Opt and Choice nodes
 * (those are just flattened)
 */
trait ASTNavigation {

  val parentAST: Attributable ==> AST = attr {case a: Attributable => findParent(a)}
  private def findParent(a: Attributable): AST =
    a.parent match {
      case o: Opt[_] => findParent(o)
      case c: Conditional[_] => findParent(c)
      case a: AST => a
      case _ => null
    }

  val prevAST: Attributable ==> AST = attr {
    case a =>
      (a.prev[Attributable]: @unchecked) match {
        case c: Choice[AST] => lastChoice(c)
        case o: One[AST] => o.value
        case a: AST => a
        case Opt(_, v: Choice[AST]) => lastChoice(v)
        case Opt(_, v: One[AST]) => v.value
        case Opt(_, v: AST) => v
        case null => {
          a.parent match {
            case o: Opt[_] => prevAST(o)
            case c: Choice[AST] => prevAST(c)
            case c: One[AST] => prevAST(c)
            case _ => null
          }
        }
      }
  }

  val nextAST: Attributable ==> AST = attr {
    case a =>
    (a.next[Attributable]: @unchecked) match {
      case c: Choice[AST] => firstChoice(c)
      case o: One[AST] => o.value
      case a: AST => a
      case Opt(_, v: Choice[AST]) => firstChoice(v)
      case Opt(_, v: One[AST]) => v.value
      case Opt(_, v: AST) => v
      case null => {
        a.parent match {
          case o: Opt[_] => nextAST(o)
          case c: Choice[AST] => nextAST(c)
          case c: One[AST] => nextAST(c)
          case _ => null
        }
      }
    }
  }

  val prevASTElems: Attributable ==> List[AST] = {
    attr {
      case null => List()
      case s => prevASTElems(prevAST(s)) ++ List(childAST(s))
    }
  }

  val nextASTElems: Attributable ==> List[AST] = {
    attr {
      case null => List()
      case s => childAST(s)::nextASTElems(nextAST(s))
    }
  }

  val childAST: Attributable ==> AST = attr {
    case a =>
      a match {
        case Opt(_, v: AST) => v
        case Opt(_, v: One[AST]) => v.value
        case Opt(_, v: Choice[AST]) => firstChoice(v)
        case x: One[AST] => x.value
        case a: AST => a
      }
  }


  /**try first prev and if that does not exist, then parent*/
  val prevOrParentAST: Attributable ==> AST = {case a: Attributable => val p = prevAST(a); if (p != null) p else parentAST(a)}

  private def prevOfChoice(c: Choice[AST]): AST = prevAST(c) match {
    case x: Choice[AST] => lastChoice(x)
    case x: AST => x
    case null => c.parent match {
      case x: Choice[AST] => prevOfChoice(x)
      case _ => null
    }
  }

  private def lastChoice[T <: AST](x: Choice[T]): T =
    x.elseBranch match {
      case c: Choice[T] => lastChoice[T](c)
      case One(c) => c
    }

  private def firstChoice[T <: AST](x: Choice[T]): T =
    x.thenBranch match {
      case c: Choice[T] => firstChoice[T](c)
      case One(c) => c
    }

  protected def outer[T](f: AST ==> T, init: () => T, e: AST): T =
    if (prevOrParentAST(e) != null) f(prevOrParentAST(e))
    else init()

  def visitAST(ast: Attributable, f: AST => Boolean): Unit = {
    val visitChildren =
      if (ast.isInstanceOf[AST])
        f(ast.asInstanceOf[AST])
      else true
        if (visitChildren)
          for (child <- ast.children)
            visitAST(child, f)
    }

}
