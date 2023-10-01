# Dry

Dry is a dynamically-typed, high-level programming language currently being written in Scala. 

<img width="875" alt="Screen Shot 2022-11-06 at 11 16 42 PM" src="https://user-images.githubusercontent.com/4519785/200179103-f6b7b544-75ae-47ea-b429-3d25f3427ae6.png">

### Contents
1. [Installation](#installation)
1. Getting Started
1. [Running the tests](#running-the-tests)
1. Examples and Tests
   - [General](https://github.com/melvic-ybanez/dry/blob/main/examples/demo.dry)
   - [Classes](https://github.com/melvic-ybanez/dry/blob/main/tests/test_class.dry) and [Constructors](https://github.com/melvic-ybanez/dry/blob/main/tests/test_init.dry)
   - [Lists](https://github.com/melvic-ybanez/dry/blob/main/tests/test_lists.dry)
   - [Modules](https://github.com/melvic-ybanez/dry/blob/main/tests/test_imports.dry)
   - More [here](https://github.com/melvic-ybanez/dry/blob/main/tests/) and [here](https://github.com/melvic-ybanez/dry/blob/main/examples/)
1. [Grammar](#grammar)

# Installation
Installation instructions will be provided after the first release.

# Running the tests
The project has a custom sbt command for running the test:
```
$ sbt testDry
```
If everything works correctly, your console should print a bunch of assertion results. The end of the logs should look like this:
```
[Success] Binding third param
=========== test_modules.dry ===========
[Success] Access variable from imported module
[Success] Access function from imported module
[Success] Modify values from imported module
[Success] Access variable from imported module
[Success] Modify values from imported module
[Success] Access method from module found via Dry Path
============= test_init.dry ============
[Success] Init should save the set variables
[Success] Init should be capable of accepting parameters
============ test_class.dry ============
[Success] Type of class
[Success] Type of instance
[Success] Ducks should quack!
[Success] Denji should say 'Woof!'
[Success] Class properties should be updated
Ran 41 tests. Successful: 41. Failed: 0.
```
The tests themselves are written in Dry (while the `testDry` command is written in Scala). You can see the directory containing them here: https://github.com/melvic-ybanez/dry/tree/main/tests. All the files in that directory that start with `test_` and have the Dry extension will be picked up by the `testDry` command.

# Grammar

The syntax of Dry should be familiar to Python and Scala developers. Here's the grammar, written in [BNF](https://en.wikipedia.org/wiki/Backus%E2%80%93Naur_form):

```bnf
<declaration> ::= <class> | <function> | <let> | <statement>
<class> ::= "class" <identifier> "{" <function>* "}"
<function> ::= "def" <identifier> <params> <block>
<let> ::= "let" <identifier> ("=" <expression>)? ";"
<statement> ::= <expr-stmt> | <block> | <if> | <while> | <for> | <return> | <import>
<expr-stmt> ::= <expression> ";"
<if> ::= "if" "(" <expression> ")" <statement> ("else" <statement>)?
<while> ::= "while" "(" <expression> ")" <statement>
<for> ::= "for" "(" (";" | <let> | <expr-stmt>) 
      (<expression>? ";") <expression> ")" <statement>
<return> ::= "return" <expression>? ";"
<import> ::= "import" <identifier> ("." <identifier>)* ";"
<expression> ::= <assignment>
<assignment> ::= (<call> ".")? <identifier> "=" <assignment> | <lambda>
<call> ::= <primary> ("(" (<expression> | ("," <expression>)*)? ")" | "." <identifier>)
<identifier> ::= <alpha> (<alpha>? <whole-num>?)*
<lambda> ::= "lambda" <params> <block> | <or>
<block> ::= "{" <declaration>* "}"
<params> ::= "(" (<identifier> | ("," <identifier>)*)? ")"
<or> ::= <and> ("or" <and>)*
<and> ::= <equality> ("and" <equality>)*
<equality> ::= <comparison> ("!=" | "==" <comparison>)*
<comparison> ::= <term> (">" | ">=" | "<" | "<=" <term>)*
<term> ::= <factor> ("-" | "+=" | "&" | "|" | "^" | "<<" | ">>" 
      | ">>>" | "<=" <factor>)*
<factor> ::= <unary> ("/" | "*" | "%" <unary>)*
<unary> ::= ("!" | "-") <expression> | <call>
<primary> ::= "false" | "true" | "none" | <number> | <string>
      | "self" | <identifier> | "(" <expression> ")"
<number> ::= <whole-num> ("." <whole-num>*)?
<string> ::= '"' (.?"\n"?)* '"'
<alpha> ::= 'a' ... 'z' | 'A' ... 'Z' 
<whole-num> ::= '0' ... '9'
```
