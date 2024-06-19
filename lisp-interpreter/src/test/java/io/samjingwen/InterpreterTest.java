package io.samjingwen;

import org.junit.jupiter.api.Test;

import java.io.IOException;

import static org.junit.jupiter.api.Assertions.*;

class InterpreterTest {

  @Test
  void testEval() throws IOException {
    Parser parser = new Parser();
    Node ast =
        parser.parse(
            """
(and
    (and true true)
    (or true false)
    true
    true
)
                """);
    Interpreter interpreter = new Interpreter();
    System.out.println(interpreter.eval(ast));
  }
}
