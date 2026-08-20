# Overview

This repo implements the MetaQASM language described in this [paper](https://www.cs.sfu.ca/~meamy/Papers/metaqasm.pdf).
The issues list all the current features being worked on. The following section details what each part of
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

# Features that have been completed
Everything listed in the typedQASM specification in the paper above has been completed.
Asides from that, circuit families taking parametric collections and qubits can be declared, but not instantiated.

Moreover, a subtyping relation on constant sized collections outside of circuit family declarations is enforced.
This implies a function that expects a collection of kind k (Qbit, Bit) and size N can be applied to a collection of kind k and size M, where M > N.
In addition,  given gates `f: Circuit(Circuit(Qbit[n + 1])), g: Circuit(Qbit[n])`, f can be applied to g as `Qbit[n]` is a supertype of `Qbit[n + 1]` (function subtyping is contravariant in the input arguments)


# TODO

## Generation of openQASM code after type checking

At the moment, the current pipeline does not generate openQASM code that is semantically equivalent to the given MetaQASM source file/s if
the files typecheck.

## Generation of helpful error messages

Given a specific error, it would be ideal if it could be transformed into an explanation describing what occurred and where it happened

## Creating an index that contains a product of two or more index variables

Indices are currently represented as a linear combination of constants and index variables. This format is unsuitable
for indices such as m * n

## Instantiating a circuit family

### Instantiating a circuit family with less/more indies than expected

### Instantiating a circuit family with the correct amount of indices, but one or more can be negative

### Instantiating a circuit family with the correct amount of indices where each is always non-negative

## Execute a gate a set number of times (for loops)

## Reversing a circuit

### Reversing an instance of a circuit family

### Reversing a constant circuit, i.e., one that does not depend on index variables

## Declaring an unscoped/scoped gate family

### Pass a circuit family as an argument

#### Pass a first order circuit family as an argument
#### Pass a higher order circuit family as an argument

### Pass a bit as an argument

### Pass a circuit as an argument

## Enforcing a subtyping relationship on circuit families

## Operating over a slice of an array

## Detecting the use free index variables outside of circuit family declarations

# Miscellaneous

## The typechecking process

Given a MetaQASM source file, it is first lexed according to the rules defined in `Lexer.x`. Once the individual tokens have been identified, terms in the language are constructed by
following the productions listed in `Grammar.y`. Afterwards, the validity of all the terms is determined by the code in `Typecheck.hs`.

## Some of the more interesting packages and how they are utilized

### [lens](https://hackage.haskell.org/package/lens)/[generic-lens](https://hackage.haskell.org/package/generic-lens)

Used for traversing nested data structures, among other things.
Look at the `test/Generators.hs` and `src/Typecheck.hs` files.


### [vary](https://hackage.haskell.org/package/vary)
Used for expressing that a term in MetaQASM is either an expression, gate, or command.
Look at the `Grammer.y` file to see how it is used

### [grisette](https://hackage.haskell.org/package/grisette)

Used for proving the validity of a parametric gate declaration.
Is only used in the `src/Typecheck.hs` file.

### [mtl](https://hackage.haskell.org/package/mtl)

Used for expressing that a theorem solver is sometimes required
for determining the type of a term.
Used mainly in `src/Typecheck.hs`

# Working with the repository

Z3 must be installed and be available in the shell path. A future goal is to add a dockerfile to the
repository containing all the dependencies needed to develop the project
