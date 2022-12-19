# Spruce - Jared Asch (jasch16) & Rohan Gupta (grohan)
Our project is Spruce, a functional lanuguage designed with concurrency support as a primary goal. Users can specify blocks of code that are guaranteed to run atomically, fork new procceses, and wait on existing ones, giving developers fine-grained control over their program's behavior. Functions are treated as first-class values, and the language supports lexically-scoped closures. 

## Language Syntax
A prorgram is defined as a sequences of `Statement`s and `FDecl`s. An `FDecl` allows for forward declaration of functions, wheres using assignment to create functions at the top-level will prevent earlier statements from using that function. The grammar can be described as follows:

```
Program := (Statement | FDecl)*

Block := (Statement)*

FDecl := func <VarName> (<FunctionArgs>) -> <VarType> { <Block> }

Statement := 
  | <LValue> = <Expression>;
  | let <VarDecl> = <Expression>;
  | if (<Expression>) { <Block> } 
  | if (<Expression>) { <Block> } else { <Block> }
  | return <Expression>;
  | <VarName>(<FunctionArgs>);
  | while (<Expression>) { <Block> }
  | for (<VarDecl> in <Expression>) { <Block> }
  | atomic { <Block> }

VarDecl := <VarName> : <VarType>

LValue := 
  | <VarName>
  | <LValue>[<Expression>]

FunctionArgs := comma-separated list of <VarDecl>

Expression := 
  | <Value>
  | <VarName>
  | <Expression>[<Expression>]
  | <Expression> <BinaryOperand> <Expression>
  | <UnaryOperand> <Expression>
  | <VarName>(<FunctionArgs>)

VarType := 
  | function
  | bool
  | int
  | string
  | [<VarType>]
```

### Values
Values can be booleans (`true`/`false`), integers, strings, arrays, or functions. Arrays must contain all elements of the same type, and are declared with a comma-separated list of array elements. Anonymous function values are declared with the following syntax:

`func (<FunctionArgs>) -> <VarType> { <Block> }`

Note that when declaring functions using `let` bindings, a semicolon must follow the function declaration, but when declared using a top-level `FDecl`, no trailing semicolon is needed. The `void` type can be used for functions that do not return anything.

### Statements
A `let` statement must be used the first time a variable is declared. This tells the interpreter to create a new variable as opposed to looking for another one in a higher scope. After a variable has been declared, it can be assigned to without providing type information

`if` and `while` statements work as they do in other languages, and will expect the guard expression to evaluate to a boolean type. If it is another type, Spruce will throw an error at runtime.

`for` statements allow for developers to iterate over arrays. The first variable declaration tells the language what variable to bind each element of the array to. If the provided expression is not an array type, Spruce will throw an error at runtime.

TODO: Function calls

When called from within a function, `return` acts as it does in most other langauges. When it is called from the top-level scope, this indicates to Spruce that the program should return that expression as the final output of the program.

Atomic blocks are used to guaranee transactional behavior. All code executed within an atomic block is guaranteed to be executed together, avoiding potential race conditions. However, because of the additional overheads required to ensure atomicity, users are more limited within atomic blocks -- functions cannot be called.

### Supported Operands
The following binary operands are supported, in order of highest to lowest precendence: array indexing with `[]`, multiplication and division, addition (`+`) and subtraction (`-`), comparison (`<, >, <=, >=`), equality checking (`!=, ==`), logical and (`and`), logical or (`or`).

The only supported unary operands are numerical negation (`-`) and boolean not (`!`).

### Native Functions
To provide certain required functionality, Spruce contains native functions that have implementations in Haskell and not in Spruce.

| Function     | Description |
| ----------- | ----------- |
| `appendFront(newEl : t, array: [t]) -> [t]` | Returns a new array with `newEl` appended to the front of `array` |
| `appendBack(newEl : t, array: [t]) -> [t]` | Returns a new array with `newEl` appended to the back of `array` |
| `len(array: [t]) -> int` | Returns the length of `array` |
| `range(n: int) -> [int]` | Returns an array of integers from 0 to n-1 | 
| `fork(f : function) -> string` | Forks `f` in a new thread, returns a thread identifier to the caller |
| `wait(thread: string) -> void` | Performs a blocking wait on the thread identified by `thread` |
| `print(val: any) -> void` | Prints `val`, exists for all primtitive types | 


## Module organization

Haskell packages typically divide their source code into three separate places:
  - Our library code is split into 4 main files. `ParseLib.hs` contains utility functions for applicative parsing, and is mostly borrowed from the HW5 file `Parser.hs`. `SpruceTypes.hs` contains common datatypes used in both our parser and interpreter, factored out to avoid circular dependendies between the two. `SpruceParser.hs` contains all logic for parsing Spruce programs to create abtract syntax tree. Finally, `SpruceEvaluator.hs` contains the logic for interpreting Spruce programs
  
  - We provide a very basic CLI to interact with Spruce, which is accessible by `stack run`. It will prompt the user and run the provided Spruce file, outputting the result. Inputting `:q` will quit the CLI.
  
  - Our test cases are broken down into parsing tests and evaluation tests. Parsing tests use the pretty print library to ensure that for arbitrary Spruce programs, pretty-printing and then parsing results in the intial program. This round-trip testing is done with QuickCheck for values, expressions, and statements. One area for improvement is including first-class functions in our QuickCheck, but we encountered issues with generating arbitrary function values.

## Building, running, and testing

This project compiles with `stack build`. You can run the CLI with `stack run`. You can run the tests with `stack test`. Finally, you can start a REPL with `stack ghci`.

## Importing additional libraries

This project is designed to run with stackage: you can easily use any library
in https://www.stackage.org/lts-19.19 by adding an entry to the
`build-depends` list of the `common-stanza` in the cabal file. If you want to
use a library that is not on stackage, you'll need to update the common-stanza
*and* add information to `stack.yaml` about where to find that library.