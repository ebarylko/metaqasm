# Structure of the repository

├── package.yaml - Lists the dependencies for the project
├── src
│   ├── Grammar.y - Dictates the grammar of the language, i.e., how tokens get parsed to form expressions in the language
│   ├── Lexer.x - Defines the tokens for the language
│   ├── Syntax.hs - Describes the possible terms in the language
│   └── Typecheck.hs - Controls how the type of a term in the language is determined
└── test
    ├── Generators.hs - Contains all the generators used in the tests (Spec.hs)
    └── Spec.hs - tests that the lexer, parser, and typechecker can properly evaluate metaQASM code


# Running the tests

Run `stack test` in the root of the repository.
