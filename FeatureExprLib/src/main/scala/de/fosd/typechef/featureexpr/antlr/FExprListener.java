// Generated from FExpr.g4 by ANTLR 4.1

package de.fosd.typechef.featureexpr.antlr;

import java.io.*;
import java.util.Stack;

import de.fosd.typechef.featureexpr.*;

import org.antlr.v4.runtime.misc.NotNull;
import org.antlr.v4.runtime.tree.ParseTreeListener;

/**
 * This interface defines a complete listener for a parse tree produced by
 * {@link FExprParser}.
 */
public interface FExprListener extends ParseTreeListener {
    /**
     * Enter a parse tree produced by {@link FExprParser#mutex}.
     *
     * @param ctx the parse tree
     */
    void enterMutex(@NotNull FExprParser.MutexContext ctx);

    /**
     * Exit a parse tree produced by {@link FExprParser#mutex}.
     *
     * @param ctx the parse tree
     */
    void exitMutex(@NotNull FExprParser.MutexContext ctx);

    /**
     * Enter a parse tree produced by {@link FExprParser#atom}.
     *
     * @param ctx the parse tree
     */
    void enterAtom(@NotNull FExprParser.AtomContext ctx);

    /**
     * Exit a parse tree produced by {@link FExprParser#atom}.
     *
     * @param ctx the parse tree
     */
    void exitAtom(@NotNull FExprParser.AtomContext ctx);

    /**
     * Enter a parse tree produced by {@link FExprParser#andExpr}.
     *
     * @param ctx the parse tree
     */
    void enterAndExpr(@NotNull FExprParser.AndExprContext ctx);

    /**
     * Exit a parse tree produced by {@link FExprParser#andExpr}.
     *
     * @param ctx the parse tree
     */
    void exitAndExpr(@NotNull FExprParser.AndExprContext ctx);

    /**
     * Enter a parse tree produced by {@link FExprParser#implication}.
     *
     * @param ctx the parse tree
     */
    void enterImplication(@NotNull FExprParser.ImplicationContext ctx);

    /**
     * Exit a parse tree produced by {@link FExprParser#implication}.
     *
     * @param ctx the parse tree
     */
    void exitImplication(@NotNull FExprParser.ImplicationContext ctx);

    /**
     * Enter a parse tree produced by {@link FExprParser#equivalence}.
     *
     * @param ctx the parse tree
     */
    void enterEquivalence(@NotNull FExprParser.EquivalenceContext ctx);

    /**
     * Exit a parse tree produced by {@link FExprParser#equivalence}.
     *
     * @param ctx the parse tree
     */
    void exitEquivalence(@NotNull FExprParser.EquivalenceContext ctx);

    /**
     * Enter a parse tree produced by {@link FExprParser#predicate}.
     *
     * @param ctx the parse tree
     */
    void enterPredicate(@NotNull FExprParser.PredicateContext ctx);

    /**
     * Exit a parse tree produced by {@link FExprParser#predicate}.
     *
     * @param ctx the parse tree
     */
    void exitPredicate(@NotNull FExprParser.PredicateContext ctx);

    /**
     * Enter a parse tree produced by {@link FExprParser#fexpr}.
     *
     * @param ctx the parse tree
     */
    void enterFexpr(@NotNull FExprParser.FexprContext ctx);

    /**
     * Exit a parse tree produced by {@link FExprParser#fexpr}.
     *
     * @param ctx the parse tree
     */
    void exitFexpr(@NotNull FExprParser.FexprContext ctx);

    /**
     * Enter a parse tree produced by {@link FExprParser#notExpr}.
     *
     * @param ctx the parse tree
     */
    void enterNotExpr(@NotNull FExprParser.NotExprContext ctx);

    /**
     * Exit a parse tree produced by {@link FExprParser#notExpr}.
     *
     * @param ctx the parse tree
     */
    void exitNotExpr(@NotNull FExprParser.NotExprContext ctx);

    /**
     * Enter a parse tree produced by {@link FExprParser#orExpr}.
     *
     * @param ctx the parse tree
     */
    void enterOrExpr(@NotNull FExprParser.OrExprContext ctx);

    /**
     * Exit a parse tree produced by {@link FExprParser#orExpr}.
     *
     * @param ctx the parse tree
     */
    void exitOrExpr(@NotNull FExprParser.OrExprContext ctx);
}