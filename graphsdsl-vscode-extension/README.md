# GraphsDSL Extension

Syntax highlighting for GraphsDSL language (`.gph` files).

## Features

- **Syntax highlighting** for keywords, operators, functions, and literals
- **Auto-closing** brackets, parentheses, and quotes
- **Comments**: Line comments (`//`) and block comments (`/* */`)
- **Code folding** for `cond`, `while`, `for` blocks
- **Custom color theme** optimized for GraphsDSL code

## How to Use

### Testing Locally

1. Open this folder in VS Code
2. Press **F5** to launch Extension Development Host
3. Open any `.gph` file (e.g., `example.gph`) to see syntax highlighting

### Installing the Extension

1. Package the extension:
   ```bash
   npm install -g @vscode/vsce
   vsce package
   ```

2. Install the `.vsix` file:
   - In VS Code: Extensions → `...` menu → Install from VSIX
   - Or run: `code --install-extension graphsdsl-0.1.0.vsix`

### Publishing to Marketplace

1. Create a publisher account at https://marketplace.visualstudio.com/
2. Update `publisher` in `package.json`
3. Run:
   ```bash
   vsce publish
   ```

## Language Features

GraphsDSL is a domain-specific language for graph algorithms with support for:

### Data Types
- **Primitives**: Int, Float, Bool, String, Node
- **Complex**: Graph (undirected, weighted), Edge, List, Queue, UnionFind

### Control Flow
- **Conditionals**: `cond ... then ... else ... end`
- **Loops**: `while ... do ... end`, `for ... in ... do ... end`
- **Commands**: `skip`, `print`

### Graph Operations
- Construction: `graph[...]`, `edge(n1, n2, weight)`
- Manipulation: `addNode`, `deleteNode`, `addEdge`, `deleteEdge`
- Set operations: `graphUnion`, `intersection`, `complement`
- Queries: `getEdges`, `adjacentNodes`
- Predicates: `esCiclico`, `esConexo`

### Edge Operations
- `getWeight`, `getNode1`, `getNode2`

### List Operations
- `len`, `head`, `tail`, `add`, `inList`, `isEmptyList`
- `sortByWeight`, `headEdge`, `tailEdges` (for edge lists)

### Queue Operations
- `queueLen`, `enqueue`, `dequeue`, `dequeueNode`, `isEmpty`

### UnionFind Operations
- `union`, `find` (for Kruskal's MST algorithm)

### Example Code

```graphsdsl
// Kruskal's Minimum Spanning Tree Algorithm
g := graph[
  ("A", [("B", 1.0), ("C", 4.0)]),
  ("B", [("A", 1.0), ("C", 2.0), ("D", 5.0)]),
  ("C", [("A", 4.0), ("B", 2.0), ("D", 3.0)]),
  ("D", [("B", 5.0), ("C", 3.0)])
];

edges := getEdges(g);
sortedEdges := sortByWeight(edges);

// Initialize UnionFind
uf := unionfind[("A", "A"), ("B", "B"), ("C", "C"), ("D", "D")];
mst := emptyList;

for e in sortedEdges do
  n1 := getNode1(e);
  n2 := getNode2(e);
  root1 := find(uf, n1);
  root2 := find(uf, n2);
  
  cond !(root1 == root2) then
    mst := add(mst, e);
    uf := union(uf, n1, n2)
  else
    skip
  end
end;

print(mst)
```

## Notes

- Graphs are **undirected** and automatically validated for symmetric edges
- Edges must have **Float** weights
- Comments use `//` for single-line or `/* */` for multi-line

