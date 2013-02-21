/*
 * TypeChef Variability-Aware Lexer.
 * Copyright 2010-2011, Christian Kaestner, Paolo Giarrusso
 * Licensed under GPL 3.0
 *
 * built on top of
 *
 * Anarres C Preprocessor
 * Copyright (c) 2007-2008, Shevek
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
 * or implied.  See the License for the specific language governing
 * permissions and limitations under the License.
 */

package de.fosd.typechef.lexer;

import de.fosd.typechef.featureexpr.FeatureModel;
import de.fosd.typechef.lexer.macrotable.MacroContext$;
import de.fosd.typechef.lexer.options.ILexerOptions;
import de.fosd.typechef.lexer.options.LexerOptions;

import java.io.*;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * (Currently a simple test class).
 */
public class Main {


    public static void main(String[] args) throws Exception {
        (new Main()).run(args, false, true, null);
    }

    public List<Token> run(String[] args, boolean returnTokenList, boolean printToStdOutput, FeatureModel featureModel) throws Exception {
        LexerOptions options = new LexerOptions();
        options.setFeatureModel(featureModel);
        options.setPrintToStdOutput(printToStdOutput);
        options.parseOptions(args);
        return run(options, returnTokenList);
    }

    public List<Token> run(ILexerOptions options, boolean returnTokenList) throws Exception {
        if (options.isPrintVersion()) {
            version(System.out);
            return new ArrayList<Token>();
        }

        //create file to dump conditions from #error directives in it
        PrintWriter   errorDirWriter = new PrintWriter( new FileWriter("output/errorDirectives.txt",true));

        Preprocessor pp = new Preprocessor(options.getFeatureModel(), errorDirWriter);

        pp.getWarnings().clear();
        pp.addWarnings(options.getWarnings());
        pp.getFeatures().clear();
        pp.addFeatures(options.getFeatures());

        pp.setListener(new PreprocessorListener(pp));
        pp.addMacro("__TYPECHEF__", FeatureExprLib.True());

        PrintWriter output = null;
        if (options.getLexOutputFile().length() > 0) {
            output = new PrintWriter(new BufferedWriter(new FileWriter(options.getLexOutputFile())));
            pp.openDebugFiles(options.getLexOutputFile());
        } else if (options.isLexPrintToStdout())
            output = new PrintWriter(new OutputStreamWriter(System.out));

        if (options.getPartialConfiguration() != null) {
            for (String def : options.getPartialConfiguration().getDefinedFeatures())
                pp.addMacro(def, FeatureExprLib.True(), "1");
            for (String undef : options.getPartialConfiguration().getUndefinedFeatures())
                pp.removeMacro(undef, FeatureExprLib.True());
        }
        for (Map.Entry<String, String> macro : options.getDefinedMacros().entrySet())
            pp.addMacro(macro.getKey(), FeatureExprLib.True(), macro.getValue());
        for (String undef : options.getUndefMacros())
            pp.removeMacro(undef, FeatureExprLib.True());

        pp.getSystemIncludePath().addAll(options.getIncludePaths());
        pp.getQuoteIncludePath().addAll(options.getQuoteIncludePath());

        for (String filter : options.getMacroFilter())
            switch (filter.charAt(0)) {
                case 'p':
                    MacroContext$.MODULE$.setPrefixFilter(filter.substring(2));
                    break;
                case 'P':
                    MacroContext$.MODULE$.setPostfixFilter(filter.substring(2));
                    break;
                case 'x':
                    MacroContext$.MODULE$.setPrefixOnlyFilter(filter.substring(2));
                    break;
                case '4':
                    MacroContext$.MODULE$.setListFilter(filter.substring(2));
                    break;
            }

        for (String include : options.getIncludedHeaders())
            pp.addInput(new File(include));


        for (String file : options.getFiles())
            pp.addInput(new FileLexerSource(new File(file)));
        if (options.getFiles().isEmpty())
            pp.addInput(new InputLexerSource(System.in));

        if (pp.getFeature(Feature.DEBUG_INCLUDEPATH)) {
            System.err.println("#" + "include \"...\" search starts here:");
            for (String dir : pp.getQuoteIncludePath())
                System.err.println("  " + dir);
            System.err.println("#" + "include <...> search starts here:");
            for (String dir : pp.getSystemIncludePath())
                System.err.println("  " + dir);
            System.err.println("End of search list.");
        }

        List<Token> resultTokenList = new ArrayList<Token>();
        int outputLine = 1;
        try {
            // TokenFilter tokenFilter = new TokenFilter();
            for (; ; ) {
                Token tok = pp.getNextToken();
                if (tok == null)
                    break;
                if (tok.getType() == Token.EOF)
                    break;


                String image = tok.getText();
                while (image.indexOf('\n') >= 0) {
                    outputLine++;
                    image = image.substring(image.indexOf('\n') + 1);
                }

                if (returnTokenList && PartialPPLexer.isResultToken(tok)) {
                    if (tok instanceof SimpleToken)
                        ((SimpleToken) tok).setLine(outputLine);
                    resultTokenList.add(tok);
                }

                if (output != null)
                    tok.lazyPrint(output);
            }
        } catch (Throwable e) {
            Preprocessor.logger.severe(e.toString());
            e.printStackTrace(System.err);
            Source s = pp.getSource();
            while (s != null) {
                System.err.println(" -> " + s);
                s = s.getParent();
            }
        } finally {
            pp.debugPreprocessorDone();
            if (output != null)
                output.flush();
            if (output != null && !options.isLexPrintToStdout())
                output.close();
        }

        errorDirWriter.close();
        return resultTokenList;
    }

    private void version(PrintStream out) {
        out.println("TypeChef " + Version.getVersion());
        out.println("This is free software.  There is NO warranty.");
    }


}
