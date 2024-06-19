package io.samjingwen;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static java.util.stream.Collectors.toList;

public class Interpreter {
  private final Map<String, Fn> functions = new HashMap<>();

  public interface Fn {
    Object call(List<Object> parameters);
  }

  public Interpreter() {
    addFunction("and", args -> args.stream().reduce(true, (x, y) -> (boolean) x && (boolean) y));
    addFunction("or", args -> args.stream().reduce(false, (x, y) -> (boolean) x || (boolean) y));
  }

  public void addFunction(String name, Fn fn) {
    functions.put(name, fn);
  }

  public Object eval(Node ast) {
    if (ast.isLeaf()) {
      return ast.value();
    }
    Fn fn = functions.get(ast.head().token());
    if (fn == null) throw new RuntimeException("Unknown function: " + ast.head().token());

    return fn.call(ast.tail().stream().map(this::eval).collect(toList()));
  }
}
