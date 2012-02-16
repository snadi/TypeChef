package de.fosd.typechef.lexer.options;

import de.fosd.typechef.featureexprUtil.FeatureModel;


public interface IFeatureModelOptions {

    FeatureModel getFeatureModel();

    FeatureModel getFeatureModelTypeSystem();

    PartialConfiguration getPartialConfiguration();
}
