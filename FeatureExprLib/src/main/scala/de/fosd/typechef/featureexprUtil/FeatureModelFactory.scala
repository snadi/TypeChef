package de.fosd.typechef.featureexprUtil

import de.fosd.typechef.featureexpr.FeatureModel


/**
 * Factory interface to create feature models
 *
 * A classname implementing this interface can be provided as command line option to the frontend
 */

trait FeatureModelFactory {
    def createFeatureModel: FeatureModel
}