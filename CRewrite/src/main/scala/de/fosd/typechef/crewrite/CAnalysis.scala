package de.fosd.typechef.crewrite

import de.fosd.typechef.parser.c._
import de.fosd.typechef.conditional._
import org.kiama._
import org.kiama.attribution._
import org.kiama.attribution.Attribution._

trait CAnalysis {

  // cf. http://www.verifysoft.com/de_cmtpp_mscoder.pdf
  val cc: Attributable ==> Int = attr { case a: Attributable => eCC(a) + 1}

  private def eCC(a: Attributable): Int = {
    a match {
      case e: IfStatement => 1 + childrenCC(e);
      case e: ElifStatement => 1 + childrenCC(e);
      case e: Conditional[_] => 1 + childrenCC(e);
      case o @ Opt(f, _) if (f.isBase()) => childrenCC(o);
      case o @ Opt(_, _) => 1 + childrenCC(o);
      case e: SwitchStatement => 1 + childrenCC(e);
      case e: CaseStatement => 1 + childrenCC(e);
      case e: DefaultStatement => 1 + childrenCC(e);
      case e: ForStatement => 1 + childrenCC(e);
      case e: WhileStatement => 1 + childrenCC(e);
      case w@List(_) => w.asInstanceOf[List[Attributable]].map(eCC(_)).foldLeft(0)(_ + _)
      case _ => 0 + childrenCC(a);
    };
  }

  private def childrenCC(a: Attributable): Int = {
    a.children.map(eCC).foldLeft(0)(_ + _)
  }
}