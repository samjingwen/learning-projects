package io.samjingwen;

import java.util.ArrayList;
import java.util.List;

public class Node {
    private final List<Node> children = new ArrayList<>();
    private final String token;

    public Node(String token) {
        this.token = token;
    }

    public void add(Node child) {
        children.add(child);
    }

    public String token() {
        return token;
    }

    public boolean value() {
        return Boolean.parseBoolean(token);
    }

    public boolean isLeaf() {
        return children.isEmpty();
    }

    public Node head() {
        return children.getFirst();
    }

    public List<Node> tail() {
        return children.subList(1, children.size());
    }

    @Override
    public String toString() {
        return isLeaf() ? token : children.toString();
    }

}
