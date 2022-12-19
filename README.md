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

### Native Functions
To provide certain functionality, Spruce contains native functions that have implementations in Haskell and not in Spruce.

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
  - The bulk of your code should be developed as a reusable library in 
    modules in the `src` directory. We've created [Lib.hs](src/Lib.hs) 
    for you to get started. You can add additional modules here.
  
  - The entry point for your executable is in [Main.hs](app/Main.hs). 
  
  - All of your test cases should be in [the test directory](test/Spec.hs).

## Building, running, and testing

This project compiles with `stack build`. 
You can run the main executable with `stack run`.
You can run the tests with `stack test`. 

Finally, you can start a REPL with `stack ghci`.

## Importing additional libraries

This project is designed to run with stackage: you can easily use any library
in https://www.stackage.org/lts-19.19 by adding an entry to the
`build-depends` list of the `common-stanza` in the cabal file. If you want to
use a library that is not on stackage, you'll need to update the common-stanza
*and* add information to `stack.yaml` about where to find that library.

## BQL Specification
A query for BixDB is written in BixDB Query Language (BQL). A BQL query consists of function declarations and a main block that is executed. Any statements that are not in a function declaration are assumed to be in the main block, and will be executed in the order specified in the query, even if divided by a function declaration. The AST is described as follows:

```
Query := [TopLevelStatement]
TopLevelStatement := FDecl | Statement

TODO
```