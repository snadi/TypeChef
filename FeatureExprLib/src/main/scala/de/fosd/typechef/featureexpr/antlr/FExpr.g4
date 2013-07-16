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
    expr {$value = $expr.value; System.out.println($value);}
    | implication {$value = $implication.value; System.out.println($value);}
    | equivalence {$value = $equivalence.value; System.out.println($value);}
    | mutex {$value = $mutex.value; System.out.println($value);}
    ;

atom returns [FeatureExpr value]:
    (NOT? DEF LEFTPARA ID RIGHTPARA{
        if($NOT != null)
            $value = toFeature($ID.getText()).not();
        else
            $value = toFeature($ID.getText());
    }
    | TRUE {$value = featureFactory.True();}
    | FALSE  {$value = featureFactory.False();}  )
    ;

expr returns [FeatureExpr value]:
    f1=atom {$value = $f1.value;}
    (
    WHITESPACE*
    (AND WHITESPACE* f2=atom {$value = $value.and($f2.value);})
    | (OR WHITESPACE* f2=atom {$value = $value.or($f2.value);})
    )*
     |  LEFTPARA expr RIGHTPARA {$value = $expr.value;}
    ;

equivalence returns [FeatureExpr value]:
   	f1=expr {$value = $f1.value;} WHITESPACE*
   	EQUIV WHITESPACE* f2=expr {$value = $value.equiv($f2.value);}
   	;

implication returns [FeatureExpr value]:
	f1=expr {$value = $f1.value;} WHITESPACE*
	IMPLIES WHITESPACE* f2=expr {$value = $value.implies($f2.value);}
	;

mutex returns [FeatureExpr value]:
    f1=expr {$value = $f1.value;} WHITESPACE*
    MEX WHITESPACE* f2=expr {$value = $value.mex($f2.value);}
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
WHITESPACE: ' '+ | '\t'+ ;
NEWLINE:'\r'? '\n' ;
NOT: '!';
LEFTPARA: '(';
RIGHTPARA: ')';
IMPLIES: '=>' | 'implies';
ID: [a-zA-Z0-9_]+  ;
COMMA: ',' ;

 