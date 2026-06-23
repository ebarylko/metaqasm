# Overview

This repo implements the MetaQASM language described in this [paper](https://www.cs.sfu.ca/~meamy/Papers/metaqasm.pdf).
The issues list all the remaining features to implement in the language. The following section details what each part of
the repository does.

# Repository breakdown

```
├── package.yaml - Lists the dependencies for the project
├── src
│   ├── Grammar.y - Dictates the grammar of the language, i.e., how tokens get parsed to form expressions in the language
│   ├── Lexer.x - Defines the tokens for the language
│   ├── Syntax.hs - Describes the possible terms in the language
│   └── Typecheck.hs - Controls how the type of a term in the language is determined
└── test
    ├── Generators.hs - Has the generators for various types of MetaQASM programs
    ├── GrammarSpec.hs - Tests that MetaQASM programs can be parsed correctly
    ├── Spec.hs - Declares where the tests can be found
    └── TypecheckSpec.hs - Tests that MetaQASM programs evaluate to a certain type
```

# Running the tests locally

Run `stack test` in the root of the repository.
