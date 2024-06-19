package io.samjingwen;

import java.io.IOException;
import java.io.PushbackReader;
import java.io.StringReader;

public class Parser {

  public Node parse(String str) throws IOException {
    try (PushbackReader reader = new PushbackReader(new StringReader(str))) {
      Node node = parse(reader);
      String rest = peek(reader);
      if (rest != null) throw new RuntimeException("Unexpected tokens: " + rest);
      return node;
    }
  }

  private Node parse(PushbackReader reader) throws IOException {
    String token = nextToken(reader);
    if (")".equals(token) || "".equals(token)) throw new RuntimeException("Unbalanced parenthesis");
    if ("(".equals(token)) {
      Node list = new Node(token);
      while (!")".equals(peek(reader))) list.add(parse(reader));
      nextToken(reader);
      return list;
    }
    return new Node(token);
  }

  private String nextToken(PushbackReader reader) throws IOException {
    String chr = peek(reader);
    return "(".equals(chr) || ")".equals(chr)
        ? String.valueOf((char) reader.read())
        : nextNode(reader);
  }

  private String nextNode(PushbackReader reader) throws IOException {
    StringBuilder buffer = new StringBuilder();
    int chr = reader.read();
    while (chr != -1 && !Character.isWhitespace(chr) && ')' != chr) {
      buffer.append((char) chr);
      chr = reader.read();
    }
    if (chr == ')') reader.unread(')');
    return buffer.toString();
  }

  private String peek(PushbackReader reader) throws IOException {
    int chr = reader.read();
    while (chr != -1 && Character.isWhitespace(chr)) chr = reader.read();
    if (chr == -1) return null;
    reader.unread(chr);
    return String.valueOf((char) chr);
  }
}
