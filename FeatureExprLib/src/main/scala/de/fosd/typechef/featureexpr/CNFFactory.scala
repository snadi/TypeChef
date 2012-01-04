package de.fosd.typechef.featureexpr

import java.util.UUID


abstract class Sign[T](v: T)

case class Pos[T](v: T) extends Sign[T](v)

case class Neg[T](v: T) extends Sign[T](v)

trait CNFFactory {

    /**
     * traverse once
     *
     * collect map from UUIDs to clause lists
     * do not recurse into known nodes
     *
     *
     */


    def rewrite(expr: FeatureExpr, init: Map[UUID, Seq[Seq[Sign[UUID]]]]): Map[UUID, Seq[Seq[Sign[UUID]]]] = {
        if (init.contains(expr.getId))
            init
        else {
            val id = expr.getId
            expr match {
                case e: DefinedExternal => init + (e.getId -> Nil)
                case DefinedMacro(_, c, _, _) => //id <=> c
                    // id => c = not id or c
                    val v1: Seq[Sign[UUID]] = Neg(id) :: Pos(c.getId) :: Nil
                    // id <= c = id or not c
                    val v2: Seq[Sign[UUID]] = Pos(id) :: Neg(c.getId) :: Nil
                    var newClauses: List[Seq[Sign[UUID]]] = v1 :: v2 :: Nil

                    var result = init + (expr.getId -> newClauses)

                    rewrite(c, result)
                case And(cl) => //id <=> c1 and c2 and ...
                    var newClauses: List[Seq[Sign[UUID]]] = Nil
                    //id => c_n
                    for (c <- cl) {
                        val v: Seq[Sign[UUID]] = Neg(id) :: Pos(c.getId) :: Nil
                        newClauses = v :: newClauses
                    }
                    //id <= c1 or c2 or ...
                    {
                        val v: Seq[Sign[UUID]] = Pos(id) :: cl.map(c => Neg(c.getId)).toList
                        newClauses = v :: newClauses
                    }

                    var result = init + (expr.getId -> newClauses)

                    for (c <- cl)
                        result = rewrite(c, result)
                    result
                case Or(cl) => //id <=> c1 or c2 or ...
                    var newClauses: List[Seq[Sign[UUID]]] = Nil
                    //id <= c_n
                    for (c <- cl) {
                        val v: Seq[Sign[UUID]] = Pos(id) :: Neg(c.getId) :: Nil
                        newClauses = v :: newClauses
                    }
                    //id => c1 or c2 or ...
                    {
                        val v: Seq[Sign[UUID]] = Neg(id) :: cl.map(c => Pos(c.getId)).toList
                        newClauses = v :: newClauses
                    }

                    var result = init + (expr.getId -> newClauses)

                    for (c <- cl)
                        result = rewrite(c, result)
                    result
                case Not(c) => //id <=> not c
                    // id => not c = not id or not c
                    val v1: Seq[Sign[UUID]] = Neg(id) :: Neg(c.getId) :: Nil
                    // id <= not c = id or c
                    val v2: Seq[Sign[UUID]] = Pos(id) :: Pos(c.getId) :: Nil
                    var newClauses: List[Seq[Sign[UUID]]] = v1 :: v2 :: Nil

                    var result = init + (expr.getId -> newClauses)

                    rewrite(c, result)
            }
        }

    }


}

object Test extends App {
    val a = FeatureExpr.createDefinedExternal("a")
    val b = FeatureExpr.createDefinedExternal("b")
    val c = FeatureExpr.createDefinedExternal("c")
    println(new CNFFactory() {}.rewrite(FeatureExpr.base, Map()).mkString("\n"))
}

