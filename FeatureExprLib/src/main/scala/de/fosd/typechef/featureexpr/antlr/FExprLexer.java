// Generated from FExpr.g4 by ANTLR 4.1

package de.fosd.typechef.featureexpr.antlr;

import java.io.*;
import java.util.Stack;

import de.fosd.typechef.featureexpr.*;

import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.TokenStream;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.misc.*;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast"})
public class FExprLexer extends Lexer {
    protected static final DFA[] _decisionToDFA;
    protected static final PredictionContextCache _sharedContextCache =
            new PredictionContextCache();
    public static final int
            ONEOF = 1, ATLEASTONEOF = 2, ATMOSTONEOF = 3, DEF = 4, EQUIV = 5, MEX = 6, OR = 7, AND = 8,
            TRUE = 9, FALSE = 10, WHITESPACE = 11, NEWLINE = 12, NOT = 13, LEFTPARA = 14, RIGHTPARA = 15,
            IMPLIES = 16, ID = 17, COMMA = 18;
    public static String[] modeNames = {
            "DEFAULT_MODE"
    };

    public static final String[] tokenNames = {
            "<INVALID>",
            "'oneOf'", "'atLeastOne'", "'atMostOne'", "DEF", "EQUIV", "MEX", "OR",
            "AND", "TRUE", "FALSE", "WHITESPACE", "NEWLINE", "'!'", "'('", "')'",
            "IMPLIES", "ID", "','"
    };
    public static final String[] ruleNames = {
            "ONEOF", "ATLEASTONEOF", "ATMOSTONEOF", "DEF", "EQUIV", "MEX", "OR", "AND",
            "TRUE", "FALSE", "WHITESPACE", "NEWLINE", "NOT", "LEFTPARA", "RIGHTPARA",
            "IMPLIES", "ID", "COMMA"
    };


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


    public FExprLexer(CharStream input) {
        super(input);
        _interp = new LexerATNSimulator(this, _ATN, _decisionToDFA, _sharedContextCache);
    }

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
    public String[] getModeNames() {
        return modeNames;
    }

    @Override
    public ATN getATN() {
        return _ATN;
    }

    @Override
    public void action(RuleContext _localctx, int ruleIndex, int actionIndex) {
        switch (ruleIndex) {
            case 10:
                WHITESPACE_action((RuleContext) _localctx, actionIndex);
                break;
        }
    }

    private void WHITESPACE_action(RuleContext _localctx, int actionIndex) {
        switch (actionIndex) {
            case 0:
                skip();
                break;
        }
    }

    public static final String _serializedATN =
            "\3\uacf5\uee8c\u4f5d\u8b0d\u4a45\u78bd\u1b2f\u3378\2\24\u00bd\b\1\4\2" +
                    "\t\2\4\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b\t\b\4\t\t\t\4\n\t\n\4" +
                    "\13\t\13\4\f\t\f\4\r\t\r\4\16\t\16\4\17\t\17\4\20\t\20\4\21\t\21\4\22" +
                    "\t\22\4\23\t\23\3\2\3\2\3\2\3\2\3\2\3\2\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3" +
                    "\3\3\3\3\3\3\3\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3\5\3\5\3\5\3\5" +
                    "\3\5\3\5\3\5\3\5\3\5\3\5\3\5\3\5\3\5\3\5\3\5\3\5\3\5\3\5\3\5\5\5V\n\5" +
                    "\3\6\3\6\3\6\3\6\3\6\3\6\3\6\3\6\5\6`\n\6\3\7\3\7\3\7\3\7\3\7\3\7\5\7" +
                    "h\n\7\3\b\3\b\3\b\3\b\3\b\5\bo\n\b\3\t\3\t\3\t\3\t\3\t\3\t\5\tw\n\t\3" +
                    "\n\3\n\3\n\3\n\3\n\3\n\3\n\3\n\3\n\3\n\3\n\3\n\3\n\5\n\u0086\n\n\3\13" +
                    "\3\13\3\13\3\13\3\13\3\13\3\13\3\13\3\13\3\13\3\13\3\13\3\13\3\13\3\13" +
                    "\3\13\5\13\u0098\n\13\3\f\6\f\u009b\n\f\r\f\16\f\u009c\3\f\3\f\3\r\5\r" +
                    "\u00a2\n\r\3\r\3\r\3\16\3\16\3\17\3\17\3\20\3\20\3\21\3\21\3\21\3\21\3" +
                    "\21\3\21\3\21\3\21\3\21\5\21\u00b5\n\21\3\22\6\22\u00b8\n\22\r\22\16\22" +
                    "\u00b9\3\23\3\23\2\24\3\3\1\5\4\1\7\5\1\t\6\1\13\7\1\r\b\1\17\t\1\21\n" +
                    "\1\23\13\1\25\f\1\27\r\2\31\16\1\33\17\1\35\20\1\37\21\1!\22\1#\23\1%" +
                    "\24\1\3\2\4\4\2\13\13\"\"\6\2\62;C\\aac|\u00ce\2\3\3\2\2\2\2\5\3\2\2\2" +
                    "\2\7\3\2\2\2\2\t\3\2\2\2\2\13\3\2\2\2\2\r\3\2\2\2\2\17\3\2\2\2\2\21\3" +
                    "\2\2\2\2\23\3\2\2\2\2\25\3\2\2\2\2\27\3\2\2\2\2\31\3\2\2\2\2\33\3\2\2" +
                    "\2\2\35\3\2\2\2\2\37\3\2\2\2\2!\3\2\2\2\2#\3\2\2\2\2%\3\2\2\2\3\'\3\2" +
                    "\2\2\5-\3\2\2\2\78\3\2\2\2\tU\3\2\2\2\13_\3\2\2\2\rg\3\2\2\2\17n\3\2\2" +
                    "\2\21v\3\2\2\2\23\u0085\3\2\2\2\25\u0097\3\2\2\2\27\u009a\3\2\2\2\31\u00a1" +
                    "\3\2\2\2\33\u00a5\3\2\2\2\35\u00a7\3\2\2\2\37\u00a9\3\2\2\2!\u00b4\3\2" +
                    "\2\2#\u00b7\3\2\2\2%\u00bb\3\2\2\2\'(\7q\2\2()\7p\2\2)*\7g\2\2*+\7Q\2" +
                    "\2+,\7h\2\2,\4\3\2\2\2-.\7c\2\2./\7v\2\2/\60\7N\2\2\60\61\7g\2\2\61\62" +
                    "\7c\2\2\62\63\7u\2\2\63\64\7v\2\2\64\65\7Q\2\2\65\66\7p\2\2\66\67\7g\2" +
                    "\2\67\6\3\2\2\289\7c\2\29:\7v\2\2:;\7O\2\2;<\7q\2\2<=\7u\2\2=>\7v\2\2" +
                    ">?\7Q\2\2?@\7p\2\2@A\7g\2\2A\b\3\2\2\2BC\7f\2\2CD\7g\2\2DE\7h\2\2EF\7" +
                    "k\2\2FG\7p\2\2GH\7g\2\2HI\7f\2\2IJ\7G\2\2JV\7z\2\2KL\7f\2\2LM\7g\2\2M" +
                    "N\7h\2\2NO\7k\2\2OP\7p\2\2PQ\7g\2\2QV\7f\2\2RS\7f\2\2ST\7g\2\2TV\7h\2" +
                    "\2UB\3\2\2\2UK\3\2\2\2UR\3\2\2\2V\n\3\2\2\2WX\7>\2\2XY\7?\2\2Y`\7@\2\2" +
                    "Z[\7g\2\2[\\\7s\2\2\\]\7w\2\2]^\7k\2\2^`\7x\2\2_W\3\2\2\2_Z\3\2\2\2`\f" +
                    "\3\2\2\2ab\7>\2\2bc\7#\2\2ch\7@\2\2de\7o\2\2ef\7g\2\2fh\7z\2\2ga\3\2\2" +
                    "\2gd\3\2\2\2h\16\3\2\2\2ij\7~\2\2jo\7~\2\2ko\7~\2\2lm\7q\2\2mo\7t\2\2" +
                    "ni\3\2\2\2nk\3\2\2\2nl\3\2\2\2o\20\3\2\2\2pq\7(\2\2qw\7(\2\2rw\7(\2\2" +
                    "st\7c\2\2tu\7p\2\2uw\7f\2\2vp\3\2\2\2vr\3\2\2\2vs\3\2\2\2w\22\3\2\2\2" +
                    "x\u0086\7\63\2\2yz\7v\2\2z{\7t\2\2{|\7w\2\2|\u0086\7g\2\2}~\7V\2\2~\177" +
                    "\7t\2\2\177\u0080\7w\2\2\u0080\u0086\7g\2\2\u0081\u0082\7V\2\2\u0082\u0083" +
                    "\7T\2\2\u0083\u0084\7W\2\2\u0084\u0086\7G\2\2\u0085x\3\2\2\2\u0085y\3" +
                    "\2\2\2\u0085}\3\2\2\2\u0085\u0081\3\2\2\2\u0086\24\3\2\2\2\u0087\u0098" +
                    "\7\62\2\2\u0088\u0089\7h\2\2\u0089\u008a\7c\2\2\u008a\u008b\7n\2\2\u008b" +
                    "\u008c\7u\2\2\u008c\u0098\7g\2\2\u008d\u008e\7H\2\2\u008e\u008f\7c\2\2" +
                    "\u008f\u0090\7n\2\2\u0090\u0091\7u\2\2\u0091\u0098\7g\2\2\u0092\u0093" +
                    "\7H\2\2\u0093\u0094\7C\2\2\u0094\u0095\7N\2\2\u0095\u0096\7U\2\2\u0096" +
                    "\u0098\7G\2\2\u0097\u0087\3\2\2\2\u0097\u0088\3\2\2\2\u0097\u008d\3\2" +
                    "\2\2\u0097\u0092\3\2\2\2\u0098\26\3\2\2\2\u0099\u009b\t\2\2\2\u009a\u0099" +
                    "\3\2\2\2\u009b\u009c\3\2\2\2\u009c\u009a\3\2\2\2\u009c\u009d\3\2\2\2\u009d" +
                    "\u009e\3\2\2\2\u009e\u009f\b\f\2\2\u009f\30\3\2\2\2\u00a0\u00a2\7\17\2" +
                    "\2\u00a1\u00a0\3\2\2\2\u00a1\u00a2\3\2\2\2\u00a2\u00a3\3\2\2\2\u00a3\u00a4" +
                    "\7\f\2\2\u00a4\32\3\2\2\2\u00a5\u00a6\7#\2\2\u00a6\34\3\2\2\2\u00a7\u00a8" +
                    "\7*\2\2\u00a8\36\3\2\2\2\u00a9\u00aa\7+\2\2\u00aa \3\2\2\2\u00ab\u00ac" +
                    "\7?\2\2\u00ac\u00b5\7@\2\2\u00ad\u00ae\7k\2\2\u00ae\u00af\7o\2\2\u00af" +
                    "\u00b0\7r\2\2\u00b0\u00b1\7n\2\2\u00b1\u00b2\7k\2\2\u00b2\u00b3\7g\2\2" +
                    "\u00b3\u00b5\7u\2\2\u00b4\u00ab\3\2\2\2\u00b4\u00ad\3\2\2\2\u00b5\"\3" +
                    "\2\2\2\u00b6\u00b8\t\3\2\2\u00b7\u00b6\3\2\2\2\u00b8\u00b9\3\2\2\2\u00b9" +
                    "\u00b7\3\2\2\2\u00b9\u00ba\3\2\2\2\u00ba$\3\2\2\2\u00bb\u00bc\7.\2\2\u00bc" +
                    "&\3\2\2\2\16\2U_gnv\u0085\u0097\u009c\u00a1\u00b4\u00b9";
    public static final ATN _ATN =
            ATNSimulator.deserialize(_serializedATN.toCharArray());

    static {
        _decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
        for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
            _decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
        }
    }
}