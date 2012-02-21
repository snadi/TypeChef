package de.fosd.typechef.lexer.options;

import de.fosd.typechef.featureexprImpl.bdd.FeatureModel;


public interface IFeatureModelOptions {

    AbstractFeatureExprModule.AbstractFeatureModel getFeatureModel();

    AbstractFeatureExprModule.AbstractFeatureModel getFeatureModelTypeSystem();

    PartialConfiguration getPartialConfiguration();
}
