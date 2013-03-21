package de.fosd.typechef.jcpp;

import de.fosd.typechef.LexerToken;
import de.fosd.typechef.lexer.LexerException;
import de.fosd.typechef.lexer.PartialPPLexer;
import de.fosd.typechef.lexer.StringLexerSource;
import org.junit.Test;

import java.io.IOException;
import java.util.List;

public class MultiParserInputTest {

    @Test
    public void testParserInput() throws LexerException, IOException {
        checkStr("#ifdef X\n" + "#define foo f\n" + "#else\n"
                + "#define foo b\n" + "#endif\n" + "bar\n" + "#ifdef B\n"
                + "foo\n" + "#endif\n", 3);
    }

    private void checkStr(String orig, int expectedNumber)
            throws LexerException, IOException {
        List<LexerToken> tokens = new PartialPPLexer().parse(new StringLexerSource(
                orig, true), null, null);
        for (LexerToken t : tokens)
            System.out.println(t);
        assert (tokens.size() == expectedNumber);

        // Assert.assertTrue("found " + result + ", but expected " + expected,
        // result.trim().endsWith(expected));
    }
}
