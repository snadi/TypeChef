package de.fosd.typechef.lexer.options;

import de.fosd.typechef.featureexprInterface.*;

public interface IFeatureModelOptions {

    AbstractFeatureExprModule.AbstractFeatureModel getFeatureModel();

    AbstractFeatureExprModule.AbstractFeatureModel getFeatureModelTypeSystem();

    PartialConfiguration getPartialConfiguration();
}
