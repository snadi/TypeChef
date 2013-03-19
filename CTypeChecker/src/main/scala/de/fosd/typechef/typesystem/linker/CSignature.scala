package de.fosd.typechef.typesystem.linker

import de.fosd.typechef.typesystem.CType
import de.fosd.typechef.featureexpr.FeatureExpr
import de.fosd.typechef.parser.Position


/**
 * signature with name type and condition. the position is only stored for debugging purposes and has no further
 * relevance.
 * its also not necessarily de/serialized
 *
 * TODO types should be selfcontained (i.e. not reference to structures or type names defined elsewhere,
 * but resolved to anonymous structs, etc.)
 */
case class CSignature(name: String, ctype: CType, fexpr: FeatureExpr, pos: Seq[Position]) {
    override def toString =
        name + ": " + ctype.toText + "\t\tif " + fexpr + "\t\tat " + pos.mkString(", ")

    def toSimpleString = name + ":" + ctype.toText

    override def hashCode = name.hashCode + ctype.hashCode()
    override def equals(that: Any) = that match {
        case CSignature(thatName, thatCType, thatFexpr, thatPos) => name == thatName && ctype == thatCType && fexpr.equivalentTo(thatFexpr) && pos == thatPos
        case _ => false
    }

    def and(f: FeatureExpr) = CSignature(name, ctype, fexpr and f, pos)

}