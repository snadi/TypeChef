package de.fosd.typechef.crewrite

import org.kiama.attribution._
import de.fosd.typechef.parser.c._
import de.fosd.typechef.conditional._

// in contrast to controlflowgraph the variabilityflowgraph
// ignores annotations and leaves reasoning about it to the
// sat solver and to the implementation using variabilityflow
// functions
trait ControlFlow {
  def succ(a: Attributable): List[AST]
  def prev(a: Attributable): List[AST]
}

trait ControlFlowImpl extends ControlFlow with ASTNavigation with ConditionalNavigation {

  // basically ignore annotations, in comparision to
  def succ(a: Attributable): List[AST] = {
    a match {
      case f@FunctionDef(_, _, _, stmt) => succ(stmt)
      case o: Opt[AST] => succ(o.entry)
      case t@ForStatement(init, break, inc, b) => {
        if (init.isDefined) List(init.get)
        else if (break.isDefined) List(break.get)
        else simpleOrCompoundStatement(t, b)
      }
      case WhileStatement(e, _) => List(e)
      case t@DoStatement(_, b) => simpleOrCompoundStatement(t, b)
      case t@IfStatement(c, _, _, _) => List(c)
      case t@ElifStatement(c, _) => List(c)
      case SwitchStatement(c, _) => List(c)
      case CompoundStatement(l) => List(l.head.entry)
      case s: Statement => nextOrFollowUp(s)
      case t => following(t)
    }
  }

  // we do not go into nested structures such as:
  // if (x) {
  //   int b;
  //   return c;
  // }
  // int d;
  // prev("int d") is "if (x) ..."
  def prev(a: Attributable): List[AST] = List(prevOrParentAST(a))

  private def nextOrFollowUp(s: AST): List[AST] = {
    val sn = nextAST(s)
    if (sn != null) List(sn)
    else followUp(s)
  }

  private def following(s: Attributable): List[AST] = {
    parentAST(s) match {
      case t@ForStatement(Some(e), c, _, b) if e.eq(s) => if (c.isDefined) List(c.get) else simpleOrCompoundStatement(t, b)
      case t@ForStatement(_, Some(e), _, b) if e.eq(s) => nextOrFollowUp(t) ++ simpleOrCompoundStatement (t, b)
      case t@ForStatement(_, c, Some(e), b) if e.eq(s) => if (c.isDefined) List(c.get) else simpleOrCompoundStatement(t, b)
      case t@ForStatement(_, c, i, e) if e.eq(s) => {
        if (i.isDefined) List(i.get)
        else if (c.isDefined) List(c.get)
        else simpleOrCompoundStatement(t, e)
      }
      case t@WhileStatement(e, b) if e.eq(s) => nextOrFollowUp(t) ++ simpleOrCompoundStatement(t, b)
      case t@DoStatement(e, b) if e.eq(s) => nextOrFollowUp(t) ++ simpleOrCompoundStatement(t, b)
      case t@IfStatement(e, tb, elif, el) if e.eq(s) => {
        var res = simpleOrCompoundStatement(t, tb) ++ nextOrFollowUp(t)
        if (!elif.isEmpty) res = res ++ List(elif.head.entry)
        if (el.isDefined) res = res ++ simpleOrCompoundStatement(t, el.get)
        res
      }
      case t@ElifStatement(e, b) if e.eq(s) => simpleOrCompoundStatement(t, b) ++ nextOrFollowUp(t)
    }
  }

  private def followUp(s: Attributable): List[AST] = {
    s.parent[Attributable] match {
      case c: CompoundStatement => followUp(c)
      case w @ WhileStatement(e, _) => List(e)
      case w: ForStatement => List(w)
      case w @ DoStatement(e, _) => List(e)
      case w @ ElifStatement(_, _) => followUp(w)
      case w @ IfStatement(_, _, _, _) => nextOrFollowUp(w)
      case o: Opt[_] => followUp(o)
      case c: Conditional[_] => followUp(c)
      case s: Statement => followUp(s)
      case _ => List()
    }
  }

  private def simpleOrCompoundStatement(p: AST, c: Conditional[_]): List[AST] = {
    c.asInstanceOf[One[_]].value match {
      case CompoundStatement(l) => if (l.isEmpty) List(p) else List(l.head.entry)
      case s: Statement => List(s)
    }
  }

  def getAllSucc(i: AST) = {
    var r = List[(AST, List[AST])]()
    var s = List(i)
    var d = List[AST]()
    var c: AST = null

    while(!s.isEmpty) {
      c = s.head
      s = s.tail

      if (d.filter(_.eq(c)).isEmpty) {
        r = (c, succ(c)) :: r
        s = s ++ r.head._2
        d = d ++ List(c)
      }
    }
    r
  }
}