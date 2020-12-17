# pythonxyc
a simple transpiler for pythonxy, a Python-like syntax that compiles down into JSX.

While a Python-to-JavaScript compiler is always fun and all, pythonxyc is best enjoyed fresh when used in the npm module `react-python`, which can be found at https://www.npmjs.com/package/react-python.


# Build and Usage
Development and building requires OCaml version 4.09.0 installed according to https://www.cs.cornell.edu/courses/cs3110/2020sp/install.html.

Build an executable binary native to your OS:
`make run`

Run the build executable binary:
`./main.byte <inputFile> <outputFile>`

### Paths
Paths are relative to the current working directory of the executable. Testing shortcuts have been removed, so it is necessary to explicitly use paths relative to the executable, for example:
`tests/parser/parse00.pyx`

# How It Works
## Overview

The lexer is defined in `lexer.mll`, which scans for patterns in the source code and matches them with the tokens defined in `grammar.mly`.

The syntax of the language and how it is parsed is defined in `grammar.mly`, which generates a `grammar.mli` interface file and `grammar.ml` parser, which in turn will generate corresponding object files. The parser associates combinations of tokens and rules (combinations of patterns) with data types specified in `ast.ml` as specified in the production rules in `grammar.mly`.

`transform.ml` takes in parsed expressions or commands (in for form of one of the many variant types as defined in `ast.ml`), carries out some transformations on them, and then writes their output to a buffer.

The `main.ml` file is the main driver of the transpiler and combines all of these together. It scans input with the lexer and passes tokens to the parser, which passes AST constructs to the transformer, which returns a buffer that `main.ml` can write to a file specified in the command-line arguments.

## Lexing Overview
To expand upon what was said above, the lexer associates specific regular expressions with tokens defined in `grammar.mly`. The lexer takes a longest-match approach in the sense that if two regular expressions (starting from the same position in the lexing buffer) match, the longer expression is accepted. The lexer accepts most, if not all, of the various types of language constructs in Python, including symbol tokens, operators, Python primitives and keywords. For a detailed list of all symbols, please visit the rules in `lexer.mll`

Strings proved difficult to handle if they contain escape sequences. Therefore, our stopgap solution was to simply enumerate a few dozen characters that a string could contain (all alphanumeric characters and simple symbols, with the exception of the backslash). Therefore, strings are theoretically much more limited in our language than in the real Python language, but this would not matter in most real-life use cases.


## Parsing Overview
