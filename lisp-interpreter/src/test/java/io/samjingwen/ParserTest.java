package io.samjingwen;

import org.junit.jupiter.api.Test;

import java.io.IOException;

import static org.junit.jupiter.api.Assertions.*;

class ParserTest {
    @Test
    void testParse() throws IOException {

        System.out.println(new Parser().parse("(and (or true false) true false)"));

    }
}
