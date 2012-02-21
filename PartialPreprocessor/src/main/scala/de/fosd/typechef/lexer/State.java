package de.fosd.typechef.lexer;

import de.fosd.typechef.featureexprJava.*;
import de.fosd.typechef.featureexprInterface.*;
import de.fosd.typechef.featureexprUtil.*;

import java.util.ArrayList;
import java.util.List;

class State {
    List<AbstractFeatureExprModule.AbstractFeatureExpr> localFeatures = new ArrayList<AbstractFeatureExprModule.AbstractFeatureExpr>();
    final State parent;

    boolean sawElse;

    /* pp */State() {
        this(null);
    }

    /* pp */State(State parent) {
        this.parent = parent;
        this.sawElse = false;
    }

    /* pp */void setSawElse() {
        clearCache();
        assert !localFeatures.isEmpty() : "else before #if?";
        sawElse = true;
        processElIf();
    }

    /* pp */boolean sawElse() {
        return sawElse;
    }

    public String toString() {
        return "State(localFeatureExpr = " + getLocalFeatureExpr() + ", currentpc=" + getFullPresenceCondition() + ", parent=" + parent
                + ", active=" + localFeatures + ", sawelse=" + sawElse + ")";
    }

    /**
     * add a feature expression to the state. first the #if expression. if
     * called again, this is interpreted as an elif expression.
     *
     * @param feature
     * @param macroTable
     */
    public void putLocalFeature(AbstractFeatureExprModule.AbstractFeatureExpr feature, FeatureProvider macroTable) {
        clearCache();
        localFeatures.add(feature);
    }

    /**
     * returns the local feature expression (explicitly negating prior features
     * from other elif branches, but not including features from outer nested
     * ifdefs)
     * <p/>
     * if this is already the else branch (sawElse is true) than the condition
     * for the else branch (negating all features) is returned
     *
     * @return
     */
    public AbstractFeatureExprModule.AbstractFeatureExpr getLocalFeatureExpr() {
        if (sawElse())
            assert !localFeatures.isEmpty() : "else before #if?";

        if (localFeatures.isEmpty())
            return FeatureExprLib.base();
        AbstractFeatureExprModule.AbstractFeatureExpr result = localFeatures.get(localFeatures.size() - 1);
        /*
           * if (sawElse) result = result.not();
           */
        for (int i = 0; i < localFeatures.size() - 1; i++)
            // result = result.and(localFeatures.get(i).not());
            result = result.and(localFeatures.get(i));

        return result;
    }

    private AbstractFeatureExprModule.AbstractFeatureExpr cache_fullPresenceCondition = null;
    private Boolean cache_isActive = null;

    /**
     * returns the full feature condition that leads to the inclusion of the
     * current token (includes all features of nested ifdefs)
     *
     * @return
     */
    public AbstractFeatureExprModule.AbstractFeatureExpr getFullPresenceCondition() {
        if (cache_fullPresenceCondition == null) {
            AbstractFeatureExprModule.AbstractFeatureExpr result = getLocalFeatureExpr();
            if (parent != null)
                result = result.and(parent.getFullPresenceCondition());
            cache_fullPresenceCondition = result;
        }
        return cache_fullPresenceCondition;
    }

    /**
     * only returns false if a code fragment is certainly dead, i.e., there is
     * no variant in which it is included.
     * <p/>
     * this can happen when a feature is explicitly undefined or explicitly
     * defined in the source code
     *
     * @param context
     * @return
     */
    public boolean isActive(AbstractFeatureExprModule.AbstractFeatureModel featureModel) {
        // check with cache and parent before using SAT solver
        if (cache_isActive != null)
            return cache_isActive.booleanValue();
        if (parent != null && parent.isCachedInactive())
            return false;
        AbstractFeatureExprModule.AbstractFeatureExpr condition = getFullPresenceCondition();
        cache_isActive = new Boolean(condition.isSatisfiable(featureModel));
        return cache_isActive.booleanValue();
    }

    private boolean isCachedInactive() {
        if (cache_isActive != null)
            return !cache_isActive.booleanValue();
        return false;
    }

    private void clearCache() {
        cache_fullPresenceCondition = null;
        cache_isActive = null;
    }

    /**
     * normally each state represents a code block if an ifdef and endif. if the
     * feature expression was base or dead, then the initial ifdef definition
     * was skipped. the skipped expression is remembered here, so that also an
     * according endif is not output
     */
    private boolean ifdefBegin = true;

    public void setNoIfdefBegin() {
        ifdefBegin = false;
    }

    public boolean hasIfdefBegin() {
        return ifdefBegin;
    }

    public void processElIf() {
        assert !localFeatures.isEmpty();
        localFeatures.set(localFeatures.size() - 1, localFeatures.get(
                localFeatures.size() - 1).not());
    }
}
