grammar FExpr;

@header{
  package de.fosd.typechef.featureexpr.antlr;
  import java.io.*;
  import java.util.Stack;
  import de.fosd.typechef.featureexpr.*;
}

@members{


AbstractFeatureExprFactory featureFactory = FeatureExprFactory$.MODULE$.dflt();
String featurenamePrefix = "";
Stack<Object> exprStack= new Stack();
enum OPERATOR{AND, OR}


//public FExpr(AbstractFeatureExprFactory featureFactory, String prefix){
//	this.featureFactory = featureFactory;
//	featurenamePrefix = prefix;
//}

private FeatureExpr toFeature(String name){
	return featureFactory.createDefinedExternal(featurenamePrefix + name);
}

}

fexpr returns [FeatureExpr value]:
    orExpr {$value = $orExpr.value;}
    | implication {$value = $implication.value;}
    | equivalence {$value = $equivalence.value;}
    | mutex {$value = $mutex.value;}
    ;

orExpr returns [FeatureExpr value]:
    andExpr {$value = $andExpr.value;}
    (OR andExpr {$value = $value.or($andExpr.value);})*
    ;

andExpr returns [FeatureExpr value]:
    predicate {$value = $predicate.value;}
    (AND predicate {$value = $value.and($predicate.value);})*
    ;

predicate returns [FeatureExpr value]:
    notExpr  {$value = $notExpr.value;}
    | atom  {$value = $atom.value;}
    ;

notExpr returns [FeatureExpr value]:
    NOT atom {$value = $atom.value.not();}
    ;

atom returns [FeatureExpr value]:
    DEF LEFTPARA ID RIGHTPARA {$value = toFeature($ID.getText());}
    | TRUE {$value = featureFactory.True();}
    | FALSE  {$value = featureFactory.False();}
    | LEFTPARA orExpr RIGHTPARA {$value = $orExpr.value;}
    ;

implication returns [FeatureExpr value]:
	f1=orExpr {$value = $f1.value;}
	IMPLIES f2=orExpr {$value = $value.implies($f2.value);}
	;

equivalence returns [FeatureExpr value]:
   	f1=orExpr {$value = $f1.value;}
   	EQUIV f2=orExpr {$value = $value.equiv($f2.value);}
   	;


mutex returns [FeatureExpr value]:
    f1=orExpr {$value = $f1.value;}
    MEX f2=orExpr {$value = $value.mex($f2.value);}
    ;

/*oneof returns [FeatureExpr value]
@init
{
        List expressions = new ArrayList();
}
:
    LEFTPARA*
    ONEOF LEFTPARA firstExpr=atom {expressions.add($firstExpr.value);}
    (COMMA otherExpr=atom)* {expressions.add($otherExpr.value);}
    RIGHTPARA {$value=
    RIGHTPARA*
    ;
 */

ONEOF: 'oneOf' ;
ATLEASTONEOF: 'atLeastOne' ;
ATMOSTONEOF: 'atMostOne';
DEF: 'definedEx' | 'defined' | 'def' ;
EQUIV: '<=>' | 'equiv' ;
MEX: '<!>' | 'mex' ;
OR: '||' | '|' | 'or';
AND: '&&' | '&' | 'and';
TRUE: '1' | 'true' | 'True' | 'TRUE'  ;
FALSE: '0' | 'false' | 'False' | 'FALSE' ;
WHITESPACE: (' ' | '\t')+  -> skip;
NEWLINE:'\r'? '\n' ;
NOT: '!';
LEFTPARA: '(';
RIGHTPARA: ')';
IMPLIES: '=>' | 'implies';
ID: [a-zA-Z0-9_]+  ;
COMMA: ',' ;

 