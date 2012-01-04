//package de.fosd.typechef.featureexpr
//
//import org.junit._
//
///**
// * Created by IntelliJ IDEA.
// * User: kaestner
// * Date: 04.01.12
// * Time: 13:51
// * To change this template use File | Settings | File Templates.
// */
//
//class CNFFactoryTest {
//    val a= FeatureExpr.createDefinedExternal("a")
//    val b= FeatureExpr.createDefinedExternal("b")
//
//    val f=new CNFFactory(){}
//
//    @Test
//    def t(){
//        Assert.assertEquals("a",f.getName(a))
//        println(f.getName(a and b))
//    }
//
//}