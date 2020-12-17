# pythonxyc
a simple transpiler for PythonXY, a Python-like syntax that compiles down into JSX.

PythonXY is Python extended with JSX to allow building React apps.

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
To expand upon what was said above, the lexer associates specific regular expressions with tokens defined in `grammar.mly`. The lexer takes a longest-match approach in the sense that if two regular expressions (starting from the same position in the lexing buffer) match, the longer expression is accepted. The lexer accepts most, if not all, of the various types of language constructs in Python, including symbol tokens, operators, Python primitives and keywords. In addition, For a detailed list of all symbols, please visit the rules in `lexer.mll`.

Comments are simply consumed and do not present any tokens to the parser.

Strings proved difficult to handle if they contained escape sequences. Therefore, our stopgap solution was to simply enumerate a few dozen characters that a string could contain (all alphanumeric characters and simple symbols, with the exception of the backslash). Therefore, strings are theoretically much more limited in our language than in the real Python language, but this would not matter in most real-life use cases. Currently, the only way to delineate strings is with double quotes.

Finally, we borrowed a couple functions of line-counting code and lexing error handling from the provided source code provided in assignment A3 in Cornell CS 4110, which contained a lexer, parser, and ast for the Imp language.


## PythonXY and Parsing Overview
PythonXY is very similar to Python. It is composed of sequences of commands padded with an arbitrary number of newlines in between. Sequences of command are recursively defined as Commands are your typical imperative language commands, such as if statements, while statements, for statements, assignments, function definitions, function calls, continue commands, and return statements. Expressions are primitives (ints, floats, booleans), strings, dictionaries, lists, and functions.

### Indentation and Program Structure
As you may have inferred, one important difference between Python and PythonXY is how indentation and scope is handled. In Python, scope is enforced by indentation. While this makes regular Python code look clean overall, we believe that enforcing indentation while having to make a decision on whether to enforce this for JSX as well was the wrong approach. Instead, we opted to use the more common approach of using specifc tokens to delineate the "opening" and "closing" of a scope. In our case, tokens that "open" a scope would be declarations for if, while, for, and functions, while the token that "closes" a scope is `@end`. We chose this token to visually match with Python directives.

### Variable Declarations
Python variables and JavaScript variables are handled differently. Python variables are simply declared by name, why nowadays JavaScript developers use the `let` and `const` keywords in their. This poses a problem for us - without an extra layer of static analysis, we would not be able to differentiate variable updates from variable declarations, and then there was the question of deciding whether a variable would be mutable or constant. Instead, we opted to use the keywords `@let` and `@const` to declare mutable and constant variables respectively, in the style of Python decorators.

## Supported Language Features
### The Basics
Comments, commands, and expressions make up a PythonXY program.

### Commands

**Assignment and Updates**
By extension, exports are supported as well. One could easily define:

`modules.exports = varName`

or even

    module.exports = {
	    "funcName1": funcName1
    }

### Expressions
### What is NOT Supported
**String Completeness**
We don't support the set of all possible strings out there, only a tiny (but still a large) subset of strings. This is a result of how we detect strings in the source code. Hopefully, we can replace our string detector with a more complete implementation.

**Classes**
Classes are not supported yet, but are coming soon! Hopefully, this should not be a huge problem. We are huge believers in React functional syntax.



## Translation Overview

There are two steps in translation: AST transformation and the translation itself.
