# GraphsDSL Extension

Syntax highlighting for GraphsDSL language (`.gph` files).

## Features

- Syntax highlighting for keywords, operators, functions, and literals
- Auto-closing brackets and quotes
- Line comments with `--`
- Code folding for `cond`, `while`, `for` blocks

## How to Use

### Testing Locally

1. Open this folder in VS Code
2. Press **F5** to launch Extension Development Host
3. Open any `.gph` file to see syntax highlighting

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

## Language Syntax

GraphsDSL is a domain-specific language for graph algorithms with support for:
- Graph, edge, queue, and list data structures
- Control flow: `cond`, `while`, `for` loops
- Graph operations: `addNode`, `addEdge`, `union`, `intersection`, etc.
- Boolean predicates: `esCiclico`, `esConexo`

Example:
```graphsdsl
g := graph[("A", [("B", 1), ("C", 2)])];
print g;
```
