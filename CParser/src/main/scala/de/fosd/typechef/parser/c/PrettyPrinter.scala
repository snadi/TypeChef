package de.fosd.typechef.parser.c

import de.fosd.typechef.conditional._
import de.fosd.typechef.featureexpr.{FeatureExprFactory, FeatureExpr}
import java.io.{FileWriter, StringWriter, Writer}

object PrettyPrinter {

    //pretty printer combinators, stolen from http://www.scala-blogs.org/2009/04/combinators-for-pretty-printers-part-1.html
    sealed abstract class Doc {
        def ~(that: Doc) = Cons(this, that)

        def ~~(that: Doc) = this ~ space ~ that

        def *(that: Doc) = this ~ line ~ that

        def ~>(that: Doc) = this ~ nest(2, line ~ that)
    }

    case object Empty extends Doc

    case object Line extends Doc

    case class Text(s: String) extends Doc

    case class Cons(left: Doc, right: Doc) extends Doc

    case class Nest(n: Int, d: Doc) extends Doc

    implicit def string(s: String): Doc = Text(s)

    val line = Line
    val space = Text(" ")
    var newLineForIfdefs = true

    def nest(n: Int, d: Doc) = Nest(n, d)

    def block(d: Doc): Doc = "{" ~> d * "}"

    def layout(d: Doc): String = d match {
        case Empty => ""
        case Line => "\n"
        case Text(s) => s
        case Cons(l, r) => layout(l) + layout(r)
        case Nest(n, Empty) => layout(Empty)
        case Nest(n, Line) => "\n" + (" " * n)
        case Nest(n, Text(s)) => layout(Text(s))
        case Nest(n, Cons(l, r)) => layout(Cons(Nest(n, l), Nest(n, r)))
        case Nest(i, Nest(j, x)) => layout(Nest(i + j, x))
    }

    // old version causing stack overflows and pretty slow
    //def print(ast: AST): String = layout(prettyPrint(ast))
    // new awesome fast version using a string writer instance
    def print(ast: AST): String = printW(ast, new StringWriter()).toString

    def layoutW(d: Doc, p: Writer): Unit = d match {
        case Empty => p.write("")
        case Line => p.write("\n")
        case Text(s) => p.write(s)
        case Cons(l, r) =>
            layoutW(l, p)
            layoutW(r, p)
        case Nest(n, Empty) => layoutW(Empty, p)
        case Nest(n, Line) => p.write("\n" + (" " * n))
        case Nest(n, Text(s)) => layoutW(Text(s), p)
        case Nest(n, Cons(l, r)) => layoutW(Cons(Nest(n, l), Nest(n, r)), p)
        case Nest(i, Nest(j, x)) => layoutW(Nest(i + j, x), p)
        case _ =>
    }

    def printW(ast: AST, writer: Writer): Writer = {
        layoutW(prettyPrint(ast), writer)
        writer
    }

    def printF(ast: AST, path: String, newLines: Boolean = true) = {
        newLineForIfdefs = newLines
        val writer = new FileWriter(path)
        layoutW(prettyPrint(ast), writer)
        writer.close()
    }


    def ppConditional(e: Conditional[_], list_feature_expr: List[FeatureExpr]): Doc = e match {
        case One(c: AST) => prettyPrint(c, list_feature_expr)
        case Choice(f, a: AST, b: AST) =>
            if (newLineForIfdefs) {
                line ~
                    "#if" ~~ f.toTextExpr *
                    prettyPrint(a, f :: list_feature_expr) *
                    "#else" *
                    prettyPrint(b, f.not :: list_feature_expr) *
                    "#endif" ~
                        line
            } else {
                "#if" ~~ f.toTextExpr *
                    prettyPrint(a, f :: list_feature_expr) *
                    "#else" *
                    prettyPrint(b, f.not :: list_feature_expr) *
                    "#endif"
            }

        case Choice(f, a: Conditional[_], b: Conditional[_]) =>
            if (newLineForIfdefs) {
                line ~
                    "#if" ~~ f.toTextExpr *
                    ppConditional(a, f :: list_feature_expr) *
                    "#else" *
                    ppConditional(b, f.not :: list_feature_expr) *
                    "#endif" ~
                        line
            } else {
                "#if" ~~ f.toTextExpr *
                    ppConditional(a, f :: list_feature_expr) *
                    "#else" *
                    ppConditional(b, f.not :: list_feature_expr) *
                    "#endif"
            }
    }

    private def optConditional(e: Opt[AST], list_feature_expr: List[FeatureExpr]): Doc = {
        if (e.feature == FeatureExprFactory.True ||
            list_feature_expr.foldLeft(FeatureExprFactory.True)(_ and _).implies(e.feature).isTautology())
            prettyPrint(e.entry, list_feature_expr)
        else if (newLineForIfdefs) {
            line ~
                "#if" ~~ e.feature.toTextExpr *
                prettyPrint(e.entry, e.feature :: list_feature_expr) *
                "#endif" ~
                    line
        } else {
            "#if" ~~ e.feature.toTextExpr *
                prettyPrint(e.entry, e.feature :: list_feature_expr) *
                "#endif"
        }

    }

    def prettyPrint(ast: AST, list_feature_expr: List[FeatureExpr] = List(FeatureExprFactory.True)): Doc = {
        implicit def pretty(a: AST): Doc = prettyPrint(a, list_feature_expr)
        implicit def prettyOpt(a: Opt[AST]): Doc = optConditional(a, list_feature_expr)
        implicit def prettyCond(a: Conditional[_]): Doc = ppConditional(a, list_feature_expr)
        implicit def prettyOptStr(a: Opt[String]): Doc = string(a.entry)

        // this method separates Opt elements of an input list variability-aware
        // problem is that when having for instance a function with one mandatory and one optional
        // parameter, e.g.,
        // void foo( int a
        // #ifdef B
        // , int B
        // #endif
        // ) {}
        // the standard sep function prints out the comma between both parameters without an
        // annotation. Further processing of the output will lead to an error.
        // This function prints out separated lists with annotated commas solving that problem.
        def sepVaware(l: List[Opt[AST]], selem: String, breakselem: Doc = space) = {
            var res: Doc = if (l.isEmpty) Empty else l.head
            var combCtx: FeatureExpr = if (l.isEmpty) FeatureExprFactory.True else l.head.feature

            for (celem <- l.drop(1)) {
                val selemfexp = combCtx.and(celem.feature)

                // separation element is never present
                if (selemfexp.isContradiction())
                    res = res ~ breakselem ~ prettyOpt(celem)

                // separation element is always present
                else if (selemfexp.isTautology())
                    res = res ~ selem ~ breakselem ~ prettyOpt(celem)

                // separation element is sometimes present
                else {
                    res = res * "#if" ~~ selemfexp.toTextExpr * selem * "#endif" * prettyOpt(celem)
                }

                // add current feature expression as it might influence the addition of selem for
                // the remaint elements of the input list l
                combCtx = combCtx.or(celem.feature)
            }

            res
        }


        def sep(l: List[Opt[AST]], s: (Doc, Doc) => Doc) = {
            val r: Doc = if (l.isEmpty) Empty else l.head
            l.drop(1).foldLeft(r)((a, b) => s(a, prettyOpt(b)))
        }
        def seps(l: List[Opt[String]], s: (Doc, Doc) => Doc) = {
            val r: Doc = if (l.isEmpty) Empty else l.head
            l.drop(1).foldLeft(r)(s(_, _))
        }
        def commaSep(l: List[Opt[AST]]) = sep(l, _ ~ "," ~~ _)
        def spaceSep(l: List[Opt[AST]]) = sep(l, _ ~~ _)
        def opt(o: Option[AST]): Doc = if (o.isDefined) o.get else Empty
        def optExt(o: Option[AST], ext: (Doc) => Doc): Doc = if (o.isDefined) ext(o.get) else Empty
        def optCondExt(o: Option[Conditional[AST]], ext: (Doc) => Doc): Doc = if (o.isDefined) ext(o.get) else Empty

        ast match {
            case TranslationUnit(ext) => sep(ext, _ * _)
            case Id(name) => name
            case Constant(v) => v
            case StringLit(v) => seps(v, _ ~~ _)
            case SimplePostfixSuffix(t) => t
            case PointerPostfixSuffix(kind, id) => kind ~ id
            case FunctionCall(params) => "(" ~ params ~ ")"
            case ArrayAccess(e) => "[" ~ e ~ "]"
            case PostfixExpr(p, s) => p ~ s
            case UnaryExpr(p, s) => p ~ s
            case SizeOfExprT(typeName) => "sizeof(" ~ typeName ~ ")"
            case SizeOfExprU(e) => "sizeof(" ~ e ~ ")"
            case CastExpr(typeName, expr) => "((" ~ typeName ~ ")" ~~ expr ~ ")"

            case PointerDerefExpr(castExpr) => "(*" ~ castExpr ~ ")"
            case PointerCreationExpr(castExpr) => "(&" ~ castExpr ~ ")"

            case UnaryOpExpr(kind, castExpr) => "(" ~ kind ~~ castExpr ~ ")"
            case NAryExpr(e, others) => "(" ~ e ~~ sep(others, _ ~~ _) ~ ")"
            case NArySubExpr(op: String, e: Expr) => op ~~ e
            case ConditionalExpr(condition: Expr, thenExpr, elseExpr: Expr) => "(" ~ condition ~~ "?" ~~ opt(thenExpr) ~~ ":" ~~ elseExpr ~ ")"
            case AssignExpr(target: Expr, operation: String, source: Expr) => "(" ~ target ~~ operation ~~ source ~ ")"
            case ExprList(exprs) => sep(exprs, _ ~~ "," ~~ _)

            case CompoundStatement(innerStatements) =>
                block(sep(innerStatements, _ * _))
            case EmptyStatement() => ";"
            case ExprStatement(expr: Expr) => expr ~ ";"
            case WhileStatement(expr: Expr, s) => "while (" ~ expr ~ ")" ~~ s
            case DoStatement(expr: Expr, s) => "do" ~~ s ~~ "while (" ~ expr ~ ")" ~ ";"
            case ForStatement(expr1, expr2, expr3, s) =>
                "for (" ~ opt(expr1) ~ ";" ~~ opt(expr2) ~ ";" ~~ opt(expr3) ~ ")" ~~ s
            case GotoStatement(target) => "goto" ~~ target ~ ";"
            case ContinueStatement() => "continue;"
            case BreakStatement() => "break;"
            case ReturnStatement(None) => "return;"
            case ReturnStatement(Some(e)) => "return" ~~ e ~ ";"
            case LabelStatement(id: Id, _) => id ~ ":"
            case CaseStatement(c: Expr) => "case" ~~ c ~ ":"
            case DefaultStatement() => "default:"
            case IfStatement(condition, thenBranch, elifs, elseBranch) =>
                "if (" ~ condition ~ ")" ~~ thenBranch ~~ sep(elifs, _ * _) ~~ optCondExt(elseBranch, line ~ "else" ~~ _)
            case ElifStatement(condition, thenBranch) => line ~ "else if (" ~ condition ~ ")" ~~ thenBranch
            case SwitchStatement(expr, s) => "switch (" ~ expr ~ ")" ~~ s
            case DeclarationStatement(decl: Declaration) => decl
            case NestedFunctionDef(isAuto, specifiers, declarator, parameters, stmt) =>
                (if (isAuto) "auto" ~~ Empty else Empty) ~ sep(specifiers, _ ~~ _) ~~ declarator ~~ sep(parameters, _ ~~ _) ~~ stmt
            case LocalLabelDeclaration(ids) => "__label__" ~~ sep(ids, _ ~ "," ~~ _) ~ ";"
            case OtherPrimitiveTypeSpecifier(typeName: String) => typeName
            case VoidSpecifier() => "void"
            case ShortSpecifier() => "short"
            case IntSpecifier() => "int"
            case FloatSpecifier() => "float"
            case LongSpecifier() => "long"
            case CharSpecifier() => "char"
            case DoubleSpecifier() => "double"

            case TypedefSpecifier() => "typedef"
            case TypeDefTypeSpecifier(name: Id) => name
            case SignedSpecifier() => "signed"
            case UnsignedSpecifier() => "unsigned"

            case InlineSpecifier() => "inline"
            case AutoSpecifier() => "auto"
            case RegisterSpecifier() => "register"
            case VolatileSpecifier() => "volatile"
            case ExternSpecifier() => "extern"
            case ConstSpecifier() => "const"
            case RestrictSpecifier() => "__restrict"
            case StaticSpecifier() => "static"

            case AtomicAttribute(n: String) => n
            case AttributeSequence(attributes) => sep(attributes, _ ~~ _)
            case CompoundAttribute(inner) => "(" ~ sep(inner, _ ~ "," ~~ _) ~ ")"

            case Declaration(declSpecs, init) =>
                sep(declSpecs, _ ~~ _) ~~ sepVaware(init, ",") ~ ";"

            case InitDeclaratorI(declarator, lst, Some(i)) =>
                if (!lst.isEmpty) {
                    declarator ~~ sep(lst, _ ~~ _) ~~ "=" ~~ i
                } else {
                    declarator ~~ "=" ~~ i
                }
            case InitDeclaratorI(declarator, lst, None) =>
                if (!lst.isEmpty) {
                    declarator ~~ sep(lst, _ ~~ _)
                } else {
                    declarator
                }
            case InitDeclaratorE(declarator, _, e: Expr) => declarator ~ ":" ~~ e

            case AtomicNamedDeclarator(pointers, id, extensions) =>
                sep(pointers, _ ~ _) ~ id ~ sep(extensions, _ ~ _)

            case NestedNamedDeclarator(pointers, nestedDecl, extensions) =>
                sep(pointers, _ ~ _) ~ "(" ~ nestedDecl ~ ")" ~ sep(extensions, _ ~ _)
            case AtomicAbstractDeclarator(pointers, extensions) =>
                sep(pointers, _ ~ _) ~ sep(extensions, _ ~ _)
            case NestedAbstractDeclarator(pointers, nestedDecl, extensions) =>
                sep(pointers, _ ~ _) ~ "(" ~ nestedDecl ~ ")" ~ sep(extensions, _ ~ _)

            case DeclIdentifierList(idList) => "(" ~ commaSep(idList) ~ ")"
            case DeclParameterDeclList(parameterDecls) => "(" ~ sepVaware(parameterDecls, ",") ~ ")"
            case DeclArrayAccess(expr) => "[" ~ opt(expr) ~ "]"
            case Initializer(initializerElementLabel, expr: Expr) => opt(initializerElementLabel) ~~ expr
            case Pointer(specifier) =>
                if (specifier.isEmpty) {
                    "*" ~ spaceSep(specifier)
                } else {
                    "*" ~ spaceSep(specifier) ~ " "
                }
            case PlainParameterDeclaration(specifiers) => spaceSep(specifiers)
            case ParameterDeclarationD(specifiers, decl) => spaceSep(specifiers) ~~ decl
            case ParameterDeclarationAD(specifiers, decl) => spaceSep(specifiers) ~~ decl
            case VarArgs() => "..."
            case EnumSpecifier(id, Some(enums)) => "enum" ~~ opt(id) ~~ block(sepVaware(enums, ",", line))
            case EnumSpecifier(Some(id), None) => "enum" ~~ id
            case Enumerator(id, Some(init)) => id ~~ "=" ~~ init
            case Enumerator(id, None) => id
            case StructOrUnionSpecifier(isUnion, id, enumerators) => (if (isUnion) "union" else "struct") ~~ opt(id) ~~ (if (enumerators.isDefined) block(sep(enumerators.get, _ * _)) else Empty)
            case StructDeclaration(qualifierList, declaratorList) => spaceSep(qualifierList) ~~ commaSep(declaratorList) ~ ";"
            case StructDeclarator(decl, initializer, _) => decl ~ optExt(initializer, ":" ~~ _)
            case StructInitializer(expr, _) => ":" ~~ expr
            case AsmExpr(isVolatile, expr) => "asm" ~~ (if (isVolatile) "volatile " else "") ~ "{" ~ expr ~ "}" ~ ";"
            case FunctionDef(specifiers, declarator, oldStyleParameters, stmt) =>
                spaceSep(specifiers) ~~ declarator ~~ spaceSep(oldStyleParameters) ~~ stmt
            case EmptyExternalDef() => ";"
            case TypelessDeclaration(declList) => commaSep(declList) ~ ";"
            case TypeName(specifiers, decl) => spaceSep(specifiers) ~~ opt(decl)

            case GnuAttributeSpecifier(attributeList) => "__attribute__((" ~ commaSep(attributeList) ~ "))"
            case AsmAttributeSpecifier(stringConst) => "__asm__( " ~ stringConst ~ ")"
            case LcurlyInitializer(inits) => "{" ~ commaSep(inits) ~ "}"
            case AlignOfExprT(typeName: TypeName) => "__alignof__(" ~ typeName ~ ")"
            case AlignOfExprU(expr: Expr) => "__alignof__" ~~ expr
            case GnuAsmExpr(isVolatile: Boolean, isAuto, expr: StringLit, stuff: Any) =>
                var ret = "asm" ~~ (if (isVolatile) "volatile " else "")
                /*if(stuff.isInstanceOf[Some[AST]] || stuff.asInstanceOf[Some[AST]].isEmpty)
                  ret = ret ~ "(" ~ expr ~~ ":" ~~ "" ~~ ")" //TODO: this is not correct: the "" should be replaced with the contents of stuff
                else*/
                ret = ret ~ "(" ~ expr ~ ")"
                ret
            case RangeExpr(from: Expr, to: Expr) => from ~~ "..." ~~ to
            case TypeOfSpecifierT(typeName: TypeName) => "typeof(" ~ typeName ~ ")"
            case TypeOfSpecifierU(e: Expr) => "typeof(" ~ e ~ ")"
            case InitializerArrayDesignator(expr: Expr) => "[" ~ expr ~ "]"
            case InitializerDesignatorD(id: Id) => "." ~ id
            case InitializerDesignatorC(id: Id) => id ~ ":"
            case InitializerAssigment(desgs) => spaceSep(desgs) ~~ "="
            case BuiltinOffsetof(typeName: TypeName, offsetofMemberDesignator) => "__builtin_offsetof(" ~ typeName ~ "," ~~ spaceSep(offsetofMemberDesignator) ~ ")"
            case OffsetofMemberDesignatorID(id: Id) => "." ~ id
            case OffsetofMemberDesignatorExpr(expr: Expr) => "[" ~ expr ~ "]"
            case BuiltinTypesCompatible(typeName1: TypeName, typeName2: TypeName) => "__builtin_types_compatible_p(" ~ typeName1 ~ "," ~~ typeName2 ~ ")"
            case BuiltinVaArgs(expr: Expr, typeName: TypeName) => "__builtin_va_arg(" ~ expr ~ "," ~~ typeName ~ ")"
            case CompoundStatementExpr(compoundStatement: CompoundStatement) => "(" ~ compoundStatement ~ ")"
            case Pragma(command: StringLit) => "_Pragma(" ~ command ~ ")"

            case e => assert(assertion = false, message = "match not exhaustive: " + e); ""
        }
    }


}