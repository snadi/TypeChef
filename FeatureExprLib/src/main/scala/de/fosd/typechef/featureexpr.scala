//Add here the needed package statements, in our case for de.fosd.typechef.featureexpr:
package de.fosd

//This object chooses at runtime the library implementation. A realistic implementation would read some configuration
//parameter, and thus support benchmarking.

//It need not be packaged together with the library, but it does need to have this name if you want to perform no
//changes on the source code.

//Any member in here is added to the content of libraryParent.

package object typechef {
  val featureexpr: featureexprInterface.AbstractFeatureExprModule =
      typechef.featureexprImpl.bdd.BDDFeatureExprLibrary
}

//In our case, this would be called:
//package object typechef {
//  val featureexpr: libraryAInterf.AbstractFEModule = ...
//}

