package de.fosd.typechef.lexer.options;

import de.fosd.typechef.featureexprImpl.bdd.FeatureModel;


public interface IFeatureModelOptions {

    FeatureModel getFeatureModel();

    FeatureModel getFeatureModelTypeSystem();

    PartialConfiguration getPartialConfiguration();
}
