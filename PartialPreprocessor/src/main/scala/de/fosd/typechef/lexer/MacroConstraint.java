package de.fosd.typechef.lexer;

import de.fosd.typechef.featureexprJava.*;
import de.fosd.typechef.featureexprInterface.*;
import de.fosd.typechef.featureexprUtil.*;

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
