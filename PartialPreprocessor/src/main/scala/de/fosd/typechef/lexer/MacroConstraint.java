package de.fosd.typechef.lexer;

import de.fosd.typechef.featureexpr.FeatureExpr;

public class MacroConstraint {

    final String name;
    final MacroConstraintKind kind;
    final AbstractFeatureExprModule.AbstractFeatureExpr expr;

    public MacroConstraint(String macroName, MacroConstraintKind kind,
                           AbstractFeatureExprModule.AbstractFeatureExpr featureExpression) {
        this.name = macroName;
        this.kind = kind;
        this.expr = featureExpression;
    }

    public enum MacroConstraintKind {
        NOTEXPANDING

    }

}
