// Generated from FExpr.g4 by ANTLR 4.1

package de.fosd.typechef.featureexpr.antlr;

import java.io.*;
import java.util.Stack;

import de.fosd.typechef.featureexpr.*;

import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.misc.*;
import org.antlr.v4.runtime.tree.*;

import java.util.List;
import java.util.Iterator;
import java.util.ArrayList;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast"})
public class FExprParser extends Parser {
    protected static final DFA[] _decisionToDFA;
    protected static final PredictionContextCache _sharedContextCache =
            new PredictionContextCache();
    public static final int
            ONEOF = 1, ATLEASTONEOF = 2, ATMOSTONEOF = 3, DEF = 4, EQUIV = 5, MEX = 6, OR = 7, AND = 8,
            TRUE = 9, FALSE = 10, WHITESPACE = 11, NEWLINE = 12, NOT = 13, LEFTPARA = 14, RIGHTPARA = 15,
            IMPLIES = 16, ID = 17, COMMA = 18;
    public static final String[] tokenNames = {
            "<INVALID>", "'oneOf'", "'atLeastOne'", "'atMostOne'", "DEF", "EQUIV",
            "MEX", "OR", "AND", "TRUE", "FALSE", "WHITESPACE", "NEWLINE", "'!'", "'('",
            "')'", "IMPLIES", "ID", "','"
    };
    public static final int
            RULE_fexpr = 0, RULE_orExpr = 1, RULE_andExpr = 2, RULE_predicate = 3,
            RULE_notExpr = 4, RULE_atom = 5, RULE_implication = 6, RULE_equivalence = 7,
            RULE_mutex = 8;
    public static final String[] ruleNames = {
            "fexpr", "orExpr", "andExpr", "predicate", "notExpr", "atom", "implication",
            "equivalence", "mutex"
    };

    @Override
    public String getGrammarFileName() {
        return "FExpr.g4";
    }

    @Override
    public String[] getTokenNames() {
        return tokenNames;
    }

    @Override
    public String[] getRuleNames() {
        return ruleNames;
    }

    @Override
    public ATN getATN() {
        return _ATN;
    }


    AbstractFeatureExprFactory featureFactory = FeatureExprFactory$.MODULE$.dflt();
    String featurenamePrefix = "";
    Stack<Object> exprStack = new Stack();

    enum OPERATOR {AND, OR}


    //public FExpr(AbstractFeatureExprFactory featureFactory, String prefix){
    //	this.featureFactory = featureFactory;
    //	featurenamePrefix = prefix;
    //}

    private FeatureExpr toFeature(String name) {
        return featureFactory.createDefinedExternal(featurenamePrefix + name);
    }


    public FExprParser(TokenStream input) {
        super(input);
        _interp = new ParserATNSimulator(this, _ATN, _decisionToDFA, _sharedContextCache);
    }

    public static class FexprContext extends ParserRuleContext {
        public FeatureExpr value;
        public OrExprContext orExpr;
        public ImplicationContext implication;
        public EquivalenceContext equivalence;
        public MutexContext mutex;

        public EquivalenceContext equivalence() {
            return getRuleContext(EquivalenceContext.class, 0);
        }

        public ImplicationContext implication() {
            return getRuleContext(ImplicationContext.class, 0);
        }

        public OrExprContext orExpr() {
            return getRuleContext(OrExprContext.class, 0);
        }

        public MutexContext mutex() {
            return getRuleContext(MutexContext.class, 0);
        }

        public FexprContext(ParserRuleContext parent, int invokingState) {
            super(parent, invokingState);
        }

        @Override
        public int getRuleIndex() {
            return RULE_fexpr;
        }

        @Override
        public void enterRule(ParseTreeListener listener) {
            if (listener instanceof FExprListener) ((FExprListener) listener).enterFexpr(this);
        }

        @Override
        public void exitRule(ParseTreeListener listener) {
            if (listener instanceof FExprListener) ((FExprListener) listener).exitFexpr(this);
        }
    }

    public final FexprContext fexpr() throws RecognitionException {
        FexprContext _localctx = new FexprContext(_ctx, getState());
        enterRule(_localctx, 0, RULE_fexpr);
        try {
            setState(30);
            switch (getInterpreter().adaptivePredict(_input, 0, _ctx)) {
                case 1:
                    enterOuterAlt(_localctx, 1);
                {
                    setState(18);
                    ((FexprContext) _localctx).orExpr = orExpr();
                    ((FexprContext) _localctx).value = ((FexprContext) _localctx).orExpr.value;
                }
                break;

                case 2:
                    enterOuterAlt(_localctx, 2);
                {
                    setState(21);
                    ((FexprContext) _localctx).implication = implication();
                    ((FexprContext) _localctx).value = ((FexprContext) _localctx).implication.value;
                }
                break;

                case 3:
                    enterOuterAlt(_localctx, 3);
                {
                    setState(24);
                    ((FexprContext) _localctx).equivalence = equivalence();
                    ((FexprContext) _localctx).value = ((FexprContext) _localctx).equivalence.value;
                }
                break;

                case 4:
                    enterOuterAlt(_localctx, 4);
                {
                    setState(27);
                    ((FexprContext) _localctx).mutex = mutex();
                    ((FexprContext) _localctx).value = ((FexprContext) _localctx).mutex.value;
                }
                break;
            }
        } catch (RecognitionException re) {
            _localctx.exception = re;
            _errHandler.reportError(this, re);
            _errHandler.recover(this, re);
        } finally {
            exitRule();
        }
        return _localctx;
    }

    public static class OrExprContext extends ParserRuleContext {
        public FeatureExpr value;
        public AndExprContext andExpr;

        public AndExprContext andExpr(int i) {
            return getRuleContext(AndExprContext.class, i);
        }

        public List<AndExprContext> andExpr() {
            return getRuleContexts(AndExprContext.class);
        }

        public List<TerminalNode> OR() {
            return getTokens(FExprParser.OR);
        }

        public TerminalNode OR(int i) {
            return getToken(FExprParser.OR, i);
        }

        public OrExprContext(ParserRuleContext parent, int invokingState) {
            super(parent, invokingState);
        }

        @Override
        public int getRuleIndex() {
            return RULE_orExpr;
        }

        @Override
        public void enterRule(ParseTreeListener listener) {
            if (listener instanceof FExprListener) ((FExprListener) listener).enterOrExpr(this);
        }

        @Override
        public void exitRule(ParseTreeListener listener) {
            if (listener instanceof FExprListener) ((FExprListener) listener).exitOrExpr(this);
        }
    }

    public final OrExprContext orExpr() throws RecognitionException {
        OrExprContext _localctx = new OrExprContext(_ctx, getState());
        enterRule(_localctx, 2, RULE_orExpr);
        int _la;
        try {
            enterOuterAlt(_localctx, 1);
            {
                setState(32);
                ((OrExprContext) _localctx).andExpr = andExpr();
                ((OrExprContext) _localctx).value = ((OrExprContext) _localctx).andExpr.value;
                setState(40);
                _errHandler.sync(this);
                _la = _input.LA(1);
                while (_la == OR) {
                    {
                        {
                            setState(34);
                            match(OR);
                            setState(35);
                            ((OrExprContext) _localctx).andExpr = andExpr();
                            ((OrExprContext) _localctx).value = _localctx.value.or(((OrExprContext) _localctx).andExpr.value);
                        }
                    }
                    setState(42);
                    _errHandler.sync(this);
                    _la = _input.LA(1);
                }
            }
        } catch (RecognitionException re) {
            _localctx.exception = re;
            _errHandler.reportError(this, re);
            _errHandler.recover(this, re);
        } finally {
            exitRule();
        }
        return _localctx;
    }

    public static class AndExprContext extends ParserRuleContext {
        public FeatureExpr value;
        public PredicateContext predicate;

        public PredicateContext predicate(int i) {
            return getRuleContext(PredicateContext.class, i);
        }

        public List<TerminalNode> AND() {
            return getTokens(FExprParser.AND);
        }

        public List<PredicateContext> predicate() {
            return getRuleContexts(PredicateContext.class);
        }

        public TerminalNode AND(int i) {
            return getToken(FExprParser.AND, i);
        }

        public AndExprContext(ParserRuleContext parent, int invokingState) {
            super(parent, invokingState);
        }

        @Override
        public int getRuleIndex() {
            return RULE_andExpr;
        }

        @Override
        public void enterRule(ParseTreeListener listener) {
            if (listener instanceof FExprListener) ((FExprListener) listener).enterAndExpr(this);
        }

        @Override
        public void exitRule(ParseTreeListener listener) {
            if (listener instanceof FExprListener) ((FExprListener) listener).exitAndExpr(this);
        }
    }

    public final AndExprContext andExpr() throws RecognitionException {
        AndExprContext _localctx = new AndExprContext(_ctx, getState());
        enterRule(_localctx, 4, RULE_andExpr);
        int _la;
        try {
            enterOuterAlt(_localctx, 1);
            {
                setState(43);
                ((AndExprContext) _localctx).predicate = predicate();
                ((AndExprContext) _localctx).value = ((AndExprContext) _localctx).predicate.value;
                setState(51);
                _errHandler.sync(this);
                _la = _input.LA(1);
                while (_la == AND) {
                    {
                        {
                            setState(45);
                            match(AND);
                            setState(46);
                            ((AndExprContext) _localctx).predicate = predicate();
                            ((AndExprContext) _localctx).value = _localctx.value.and(((AndExprContext) _localctx).predicate.value);
                        }
                    }
                    setState(53);
                    _errHandler.sync(this);
                    _la = _input.LA(1);
                }
            }
        } catch (RecognitionException re) {
            _localctx.exception = re;
            _errHandler.reportError(this, re);
            _errHandler.recover(this, re);
        } finally {
            exitRule();
        }
        return _localctx;
    }

    public static class PredicateContext extends ParserRuleContext {
        public FeatureExpr value;
        public NotExprContext notExpr;
        public AtomContext atom;

        public NotExprContext notExpr() {
            return getRuleContext(NotExprContext.class, 0);
        }

        public AtomContext atom() {
            return getRuleContext(AtomContext.class, 0);
        }

        public PredicateContext(ParserRuleContext parent, int invokingState) {
            super(parent, invokingState);
        }

        @Override
        public int getRuleIndex() {
            return RULE_predicate;
        }

        @Override
        public void enterRule(ParseTreeListener listener) {
            if (listener instanceof FExprListener) ((FExprListener) listener).enterPredicate(this);
        }

        @Override
        public void exitRule(ParseTreeListener listener) {
            if (listener instanceof FExprListener) ((FExprListener) listener).exitPredicate(this);
        }
    }

    public final PredicateContext predicate() throws RecognitionException {
        PredicateContext _localctx = new PredicateContext(_ctx, getState());
        enterRule(_localctx, 6, RULE_predicate);
        try {
            setState(60);
            switch (_input.LA(1)) {
                case NOT:
                    enterOuterAlt(_localctx, 1);
                {
                    setState(54);
                    ((PredicateContext) _localctx).notExpr = notExpr();
                    ((PredicateContext) _localctx).value = ((PredicateContext) _localctx).notExpr.value;
                }
                break;
                case DEF:
                case TRUE:
                case FALSE:
                case LEFTPARA:
                    enterOuterAlt(_localctx, 2);
                {
                    setState(57);
                    ((PredicateContext) _localctx).atom = atom();
                    ((PredicateContext) _localctx).value = ((PredicateContext) _localctx).atom.value;
                }
                break;
                default:
                    throw new NoViableAltException(this);
            }
        } catch (RecognitionException re) {
            _localctx.exception = re;
            _errHandler.reportError(this, re);
            _errHandler.recover(this, re);
        } finally {
            exitRule();
        }
        return _localctx;
    }

    public static class NotExprContext extends ParserRuleContext {
        public FeatureExpr value;
        public AtomContext atom;

        public TerminalNode NOT() {
            return getToken(FExprParser.NOT, 0);
        }

        public AtomContext atom() {
            return getRuleContext(AtomContext.class, 0);
        }

        public NotExprContext(ParserRuleContext parent, int invokingState) {
            super(parent, invokingState);
        }

        @Override
        public int getRuleIndex() {
            return RULE_notExpr;
        }

        @Override
        public void enterRule(ParseTreeListener listener) {
            if (listener instanceof FExprListener) ((FExprListener) listener).enterNotExpr(this);
        }

        @Override
        public void exitRule(ParseTreeListener listener) {
            if (listener instanceof FExprListener) ((FExprListener) listener).exitNotExpr(this);
        }
    }

    public final NotExprContext notExpr() throws RecognitionException {
        NotExprContext _localctx = new NotExprContext(_ctx, getState());
        enterRule(_localctx, 8, RULE_notExpr);
        try {
            enterOuterAlt(_localctx, 1);
            {
                setState(62);
                match(NOT);
                setState(63);
                ((NotExprContext) _localctx).atom = atom();
                ((NotExprContext) _localctx).value = ((NotExprContext) _localctx).atom.value.not();
            }
        } catch (RecognitionException re) {
            _localctx.exception = re;
            _errHandler.reportError(this, re);
            _errHandler.recover(this, re);
        } finally {
            exitRule();
        }
        return _localctx;
    }

    public static class AtomContext extends ParserRuleContext {
        public FeatureExpr value;
        public Token ID;
        public OrExprContext orExpr;

        public TerminalNode DEF() {
            return getToken(FExprParser.DEF, 0);
        }

        public TerminalNode TRUE() {
            return getToken(FExprParser.TRUE, 0);
        }

        public TerminalNode LEFTPARA() {
            return getToken(FExprParser.LEFTPARA, 0);
        }

        public TerminalNode RIGHTPARA() {
            return getToken(FExprParser.RIGHTPARA, 0);
        }

        public TerminalNode ID() {
            return getToken(FExprParser.ID, 0);
        }

        public OrExprContext orExpr() {
            return getRuleContext(OrExprContext.class, 0);
        }

        public TerminalNode FALSE() {
            return getToken(FExprParser.FALSE, 0);
        }

        public AtomContext(ParserRuleContext parent, int invokingState) {
            super(parent, invokingState);
        }

        @Override
        public int getRuleIndex() {
            return RULE_atom;
        }

        @Override
        public void enterRule(ParseTreeListener listener) {
            if (listener instanceof FExprListener) ((FExprListener) listener).enterAtom(this);
        }

        @Override
        public void exitRule(ParseTreeListener listener) {
            if (listener instanceof FExprListener) ((FExprListener) listener).exitAtom(this);
        }
    }

    public final AtomContext atom() throws RecognitionException {
        AtomContext _localctx = new AtomContext(_ctx, getState());
        enterRule(_localctx, 10, RULE_atom);
        try {
            setState(80);
            switch (_input.LA(1)) {
                case DEF:
                    enterOuterAlt(_localctx, 1);
                {
                    setState(66);
                    match(DEF);
                    setState(67);
                    match(LEFTPARA);
                    setState(68);
                    ((AtomContext) _localctx).ID = match(ID);
                    setState(69);
                    match(RIGHTPARA);
                    ((AtomContext) _localctx).value = toFeature(((AtomContext) _localctx).ID.getText());
                }
                break;
                case TRUE:
                    enterOuterAlt(_localctx, 2);
                {
                    setState(71);
                    match(TRUE);
                    ((AtomContext) _localctx).value = featureFactory.True();
                }
                break;
                case FALSE:
                    enterOuterAlt(_localctx, 3);
                {
                    setState(73);
                    match(FALSE);
                    ((AtomContext) _localctx).value = featureFactory.False();
                }
                break;
                case LEFTPARA:
                    enterOuterAlt(_localctx, 4);
                {
                    setState(75);
                    match(LEFTPARA);
                    setState(76);
                    ((AtomContext) _localctx).orExpr = orExpr();
                    setState(77);
                    match(RIGHTPARA);
                    ((AtomContext) _localctx).value = ((AtomContext) _localctx).orExpr.value;
                }
                break;
                default:
                    throw new NoViableAltException(this);
            }
        } catch (RecognitionException re) {
            _localctx.exception = re;
            _errHandler.reportError(this, re);
            _errHandler.recover(this, re);
        } finally {
            exitRule();
        }
        return _localctx;
    }

    public static class ImplicationContext extends ParserRuleContext {
        public FeatureExpr value;
        public OrExprContext f1;
        public OrExprContext f2;

        public List<OrExprContext> orExpr() {
            return getRuleContexts(OrExprContext.class);
        }

        public TerminalNode IMPLIES() {
            return getToken(FExprParser.IMPLIES, 0);
        }

        public OrExprContext orExpr(int i) {
            return getRuleContext(OrExprContext.class, i);
        }

        public ImplicationContext(ParserRuleContext parent, int invokingState) {
            super(parent, invokingState);
        }

        @Override
        public int getRuleIndex() {
            return RULE_implication;
        }

        @Override
        public void enterRule(ParseTreeListener listener) {
            if (listener instanceof FExprListener) ((FExprListener) listener).enterImplication(this);
        }

        @Override
        public void exitRule(ParseTreeListener listener) {
            if (listener instanceof FExprListener) ((FExprListener) listener).exitImplication(this);
        }
    }

    public final ImplicationContext implication() throws RecognitionException {
        ImplicationContext _localctx = new ImplicationContext(_ctx, getState());
        enterRule(_localctx, 12, RULE_implication);
        try {
            enterOuterAlt(_localctx, 1);
            {
                setState(82);
                ((ImplicationContext) _localctx).f1 = orExpr();
                ((ImplicationContext) _localctx).value = ((ImplicationContext) _localctx).f1.value;
                setState(84);
                match(IMPLIES);
                setState(85);
                ((ImplicationContext) _localctx).f2 = orExpr();
                ((ImplicationContext) _localctx).value = _localctx.value.implies(((ImplicationContext) _localctx).f2.value);
            }
        } catch (RecognitionException re) {
            _localctx.exception = re;
            _errHandler.reportError(this, re);
            _errHandler.recover(this, re);
        } finally {
            exitRule();
        }
        return _localctx;
    }

    public static class EquivalenceContext extends ParserRuleContext {
        public FeatureExpr value;
        public OrExprContext f1;
        public OrExprContext f2;

        public List<OrExprContext> orExpr() {
            return getRuleContexts(OrExprContext.class);
        }

        public OrExprContext orExpr(int i) {
            return getRuleContext(OrExprContext.class, i);
        }

        public TerminalNode EQUIV() {
            return getToken(FExprParser.EQUIV, 0);
        }

        public EquivalenceContext(ParserRuleContext parent, int invokingState) {
            super(parent, invokingState);
        }

        @Override
        public int getRuleIndex() {
            return RULE_equivalence;
        }

        @Override
        public void enterRule(ParseTreeListener listener) {
            if (listener instanceof FExprListener) ((FExprListener) listener).enterEquivalence(this);
        }

        @Override
        public void exitRule(ParseTreeListener listener) {
            if (listener instanceof FExprListener) ((FExprListener) listener).exitEquivalence(this);
        }
    }

    public final EquivalenceContext equivalence() throws RecognitionException {
        EquivalenceContext _localctx = new EquivalenceContext(_ctx, getState());
        enterRule(_localctx, 14, RULE_equivalence);
        try {
            enterOuterAlt(_localctx, 1);
            {
                setState(88);
                ((EquivalenceContext) _localctx).f1 = orExpr();
                ((EquivalenceContext) _localctx).value = ((EquivalenceContext) _localctx).f1.value;
                setState(90);
                match(EQUIV);
                setState(91);
                ((EquivalenceContext) _localctx).f2 = orExpr();
                ((EquivalenceContext) _localctx).value = _localctx.value.equiv(((EquivalenceContext) _localctx).f2.value);
            }
        } catch (RecognitionException re) {
            _localctx.exception = re;
            _errHandler.reportError(this, re);
            _errHandler.recover(this, re);
        } finally {
            exitRule();
        }
        return _localctx;
    }

    public static class MutexContext extends ParserRuleContext {
        public FeatureExpr value;
        public OrExprContext f1;
        public OrExprContext f2;

        public List<OrExprContext> orExpr() {
            return getRuleContexts(OrExprContext.class);
        }

        public TerminalNode MEX() {
            return getToken(FExprParser.MEX, 0);
        }

        public OrExprContext orExpr(int i) {
            return getRuleContext(OrExprContext.class, i);
        }

        public MutexContext(ParserRuleContext parent, int invokingState) {
            super(parent, invokingState);
        }

        @Override
        public int getRuleIndex() {
            return RULE_mutex;
        }

        @Override
        public void enterRule(ParseTreeListener listener) {
            if (listener instanceof FExprListener) ((FExprListener) listener).enterMutex(this);
        }

        @Override
        public void exitRule(ParseTreeListener listener) {
            if (listener instanceof FExprListener) ((FExprListener) listener).exitMutex(this);
        }
    }

    public final MutexContext mutex() throws RecognitionException {
        MutexContext _localctx = new MutexContext(_ctx, getState());
        enterRule(_localctx, 16, RULE_mutex);
        try {
            enterOuterAlt(_localctx, 1);
            {
                setState(94);
                ((MutexContext) _localctx).f1 = orExpr();
                ((MutexContext) _localctx).value = ((MutexContext) _localctx).f1.value;
                setState(96);
                match(MEX);
                setState(97);
                ((MutexContext) _localctx).f2 = orExpr();
                ((MutexContext) _localctx).value = _localctx.value.mex(((MutexContext) _localctx).f2.value);
            }
        } catch (RecognitionException re) {
            _localctx.exception = re;
            _errHandler.reportError(this, re);
            _errHandler.recover(this, re);
        } finally {
            exitRule();
        }
        return _localctx;
    }

    public static final String _serializedATN =
            "\3\uacf5\uee8c\u4f5d\u8b0d\u4a45\u78bd\u1b2f\u3378\3\24g\4\2\t\2\4\3\t" +
                    "\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b\t\b\4\t\t\t\4\n\t\n\3\2\3\2\3\2" +
                    "\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\5\2!\n\2\3\3\3\3\3\3\3\3\3\3\3\3" +
                    "\7\3)\n\3\f\3\16\3,\13\3\3\4\3\4\3\4\3\4\3\4\3\4\7\4\64\n\4\f\4\16\4\67" +
                    "\13\4\3\5\3\5\3\5\3\5\3\5\3\5\5\5?\n\5\3\6\3\6\3\6\3\6\3\7\3\7\3\7\3\7" +
                    "\3\7\3\7\3\7\3\7\3\7\3\7\3\7\3\7\3\7\3\7\5\7S\n\7\3\b\3\b\3\b\3\b\3\b" +
                    "\3\b\3\t\3\t\3\t\3\t\3\t\3\t\3\n\3\n\3\n\3\n\3\n\3\n\3\n\2\13\2\4\6\b" +
                    "\n\f\16\20\22\2\2f\2 \3\2\2\2\4\"\3\2\2\2\6-\3\2\2\2\b>\3\2\2\2\n@\3\2" +
                    "\2\2\fR\3\2\2\2\16T\3\2\2\2\20Z\3\2\2\2\22`\3\2\2\2\24\25\5\4\3\2\25\26" +
                    "\b\2\1\2\26!\3\2\2\2\27\30\5\16\b\2\30\31\b\2\1\2\31!\3\2\2\2\32\33\5" +
                    "\20\t\2\33\34\b\2\1\2\34!\3\2\2\2\35\36\5\22\n\2\36\37\b\2\1\2\37!\3\2" +
                    "\2\2 \24\3\2\2\2 \27\3\2\2\2 \32\3\2\2\2 \35\3\2\2\2!\3\3\2\2\2\"#\5\6" +
                    "\4\2#*\b\3\1\2$%\7\t\2\2%&\5\6\4\2&\'\b\3\1\2\')\3\2\2\2($\3\2\2\2),\3" +
                    "\2\2\2*(\3\2\2\2*+\3\2\2\2+\5\3\2\2\2,*\3\2\2\2-.\5\b\5\2.\65\b\4\1\2" +
                    "/\60\7\n\2\2\60\61\5\b\5\2\61\62\b\4\1\2\62\64\3\2\2\2\63/\3\2\2\2\64" +
                    "\67\3\2\2\2\65\63\3\2\2\2\65\66\3\2\2\2\66\7\3\2\2\2\67\65\3\2\2\289\5" +
                    "\n\6\29:\b\5\1\2:?\3\2\2\2;<\5\f\7\2<=\b\5\1\2=?\3\2\2\2>8\3\2\2\2>;\3" +
                    "\2\2\2?\t\3\2\2\2@A\7\17\2\2AB\5\f\7\2BC\b\6\1\2C\13\3\2\2\2DE\7\6\2\2" +
                    "EF\7\20\2\2FG\7\23\2\2GH\7\21\2\2HS\b\7\1\2IJ\7\13\2\2JS\b\7\1\2KL\7\f" +
                    "\2\2LS\b\7\1\2MN\7\20\2\2NO\5\4\3\2OP\7\21\2\2PQ\b\7\1\2QS\3\2\2\2RD\3" +
                    "\2\2\2RI\3\2\2\2RK\3\2\2\2RM\3\2\2\2S\r\3\2\2\2TU\5\4\3\2UV\b\b\1\2VW" +
                    "\7\22\2\2WX\5\4\3\2XY\b\b\1\2Y\17\3\2\2\2Z[\5\4\3\2[\\\b\t\1\2\\]\7\7" +
                    "\2\2]^\5\4\3\2^_\b\t\1\2_\21\3\2\2\2`a\5\4\3\2ab\b\n\1\2bc\7\b\2\2cd\5" +
                    "\4\3\2de\b\n\1\2e\23\3\2\2\2\7 *\65>R";
    public static final ATN _ATN =
            ATNSimulator.deserialize(_serializedATN.toCharArray());

    static {
        _decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
        for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
            _decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
        }
    }
}