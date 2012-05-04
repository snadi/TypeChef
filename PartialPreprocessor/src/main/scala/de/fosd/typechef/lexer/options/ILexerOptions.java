package de.fosd.typechef.lexer.options;

import de.fosd.typechef.lexer.Feature;
import de.fosd.typechef.lexer.Warning;

import java.util.List;
import java.util.Map;
import java.util.Set;

public interface ILexerOptions extends IFeatureModelOptions {

    Map<String, String> getDefinedMacros();

    Set<String> getUndefMacros();

    List<String> getIncludePaths();

    List<String> getQuoteIncludePath();

    List<String> getMacroFilter();

    List<String> getIncludedHeaders();

    String getLexOutputFile();

    boolean isPrintVersion();


    Set<Warning> getWarnings();

    Set<Feature> getFeatures();

    List<String> getFiles();

    boolean isLexPrintToStdout();
}