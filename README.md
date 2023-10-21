# Dry

Dry is a dynamically-typed, high-level programming language currently being written in Scala.

The image below shows an overview of Dry's syntax via examples. You can learn more about the 
language [here](#contents).

<img width="875" alt="Screen Shot 2022-11-06 at 11 16 42 PM" src="https://user-images.githubusercontent.com/4519785/200179103-f6b7b544-75ae-47ea-b429-3d25f3427ae6.png">

### Contents
1. [Introduction](#introduction)
   - [What is Dry](#what-is-dry)
   - [Why Use Dry](#why-use-dry)
1. [Installation](#installation)
1. [Getting Started](#getting-started)
   - [Starting the REPL](#starting-the-repl)
   - [Running a Dry Script](#running-a-dry-script)
1. [Running the tests](#running-the-tests)
1. Examples and Tests
   - [General](https://github.com/melvic-ybanez/dry/blob/main/examples/demo.dry)
   - [Classes](https://github.com/melvic-ybanez/dry/blob/main/tests/test_class.dry) and [Constructors](https://github.com/melvic-ybanez/dry/blob/main/tests/test_init.dry)
   - [Lists](https://github.com/melvic-ybanez/dry/blob/main/tests/test_lists.dry)
   - [Modules](https://github.com/melvic-ybanez/dry/blob/main/tests/test_imports.dry)
   - More [here](https://github.com/melvic-ybanez/dry/blob/main/tests/) and [here](https://github.com/melvic-ybanez/dry/blob/main/examples/)
1. [Grammar](#grammar)

# Introduction

## What is Dry

You can think of Dry as a Python-like programming language with curly braces and 
better support for functional programming (e.g. multi-line lambdas and partial 
function application). Dry is both _dynamically_ and _strongly_ typed, just like Python.

Dry was also heavily influenced by the Lox language, and you'll see why in the next section.

The name doesn't actually mean "Don't Repeat Yourself" (though maybe it can be a good slogan).
Dry was rather named after the eye condition from which I suffered for about 2 years.

## Why Use Dry

Dry started as a hobby project, when I was going through the first part of
[Crafting Interpreters](https://craftinginterpreters.com/). This is how Lox, the object
language in that book, had also influenced the design of Dry.

However, as Dry started to grow, it became more and more of a language
for cases where Python would normally shine. 
It became a language for people like myself back then (when I used Python at work), 
who would sometimes wish Python had multi-line lambdas, supported braces and not overly rely on indentations.

Of course Dry doesn't have the libraries and tools that Python has, but if the following
are true about you or your requirements, then you might want to give Dry a try:
1. You want an expressive language to write scripts, and you don't need the help of a static type system.
1. You don't need much tools and libraries for your project.
1. You like Python but want to use first-class functions a lot.

# Installation

1. The easiest way to install Dry on your system is to download a Dry executable (a Jar file) from the [releases](https://github.com/melvic-ybanez/dry/releases) page. 
   I recommend you choose the one from the latest release. You can put the jar file in a directory of your choosing.
1. Install [Java](https://www.java.com/en/), if you haven't already. 
1. Since Dry is a Scala application, it is compiled to Java bytecode. This means you can
   run the Jar file you downloaded in the previous section the same way you run any Java
   Jar application, using the `java -jar` command.

   Go to the downloaded jar's directory and enter the following:

   ```shell
   $ java -jar dry-<version>.jar
   ```

   where `version` is the version of the release you downloaded. You should see a 
   welcome message in the screen:

   ```
   Welcome to Dry.
   Type in expressions and statements for evaluation. Type 'exit' to quit.
   dry> 
   ```

   You are now ready to start playing with Dry.

# Getting Started

In Dry, like in some languages, you can either start a REPL, or run a script. This section will show you both.

## Starting the REPL

The welcome message you saw at the end of the [installation](#installation) section indicates that
you have entered the REPL (read-eval-print-loop) environment. That's what would
happen if you ran Dry without specifying a path to a Dry script. 

Many languages such as Scala, Python, Haskell, or any of the known Lisp dialects like Clojure, 
have their own supports for REPLs, so programmers coming from a background of any of 
these languages might already be familiar with it.

The REPL allows you to enter expressions and declarations, and see their evaluated results immediately:

```shell
dry> 100 + 20 - 8 * 5
80
dry> "hello" + "world"
helloworld
dry> let x = 10;
dry> x * x
100
dry> def sum(x, y) { return x + y; }
dry> sum(10, 9)
19
dry> sum(11, (lambda(y) { return y * y; })(2))
15
dry> let one_plus = sum(1, _);   // partial application
dry> one_plus(5)
6
dry> one_plus(6)
7
dry> 
```

To quit the REPL, enter `exit`:

```shell
dry> exit
Bye!
```

## Running a Dry Script

To run a Dry script, you need to save the script to a file first and pass its path
as an argument to the jar command. For instance, the code you saw at the beginning 
of this ReadMe file is available in `examples/class_demo.dry` under this repository. We can
try running that with Dry:

```shell
$ java -jar dry-<version>.jar <path-to-repo>/examples/class_demo.dry
quack!
Woof
quack!
1 1 2 3 5
```

As shown above, Dry source files end with the `.dry`.

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

There is also a set of tests written in Scala, and you can run them like normal
Scala tests:

```shell
$ sbt test
```

# Grammar

The syntax of Dry should be familiar to Python and Scala developers. Here's the grammar, written in [BNF](https://en.wikipedia.org/wiki/Backus%E2%80%93Naur_form):

```bnf
<declaration> ::= <class> | <function> | <let> | <statement>
<class>       ::= "class" <identifier> "{" <function>* "}"
<function>    ::= "def" <identifier> <params> <block>
<let>         ::= "let" <identifier> ("=" <expression>)? ";"
<statement>   ::= <expr-stmt> | <block> | <if> | <while> | <for> | <return> | <import>
<expr-stmt>   ::= <expression> ";"
<if>          ::= "if" "(" <expression> ")" <statement> ("else" <statement>)?
<while>       ::= "while" "(" <expression> ")" <statement>
<for>         ::= "for" "(" (";" | <let> | <expr-stmt>)
      (<expression>? ";") <expression> ")" <statement>
<return>      ::= "return" <expression>? ";"
<import>      ::= "import" <identifier>("."<identifier>)* ";"
<expression>  ::= <assignment>
<assignment>  ::= (<call>".")?<identifier> "=" <assignment> | <lambda>
<call>        ::= <primary> ("(" (<expression> | ("," <expression>)*)? ")" | "." <identifier>)
<identifier>  ::= <alpha>(<alpha>?<digit>?)*
<lambda>      ::= "lambda" <params> <block> | <or>
<block>       ::= "{" <declaration>* "}"
<params>      ::= "(" (<identifier> | ("," <identifier>)*)? ")"
<or>          ::= <and> ("or" <and>)*
<and>         ::= <equality> ("and" <equality>)*
<equality>    ::= <comparison> ("!=" | "==" <comparison>)*
<comparison>  ::= <term> (">" | ">=" | "<" | "<=" <term>)*
<term>        ::= <factor> ("-" | "+" | "&" | "|" | "^" | "<<" | ">>"
      | ">>>" | "<=" <factor>)*
<factor>      ::= <unary> ("/" | "*" | "%" <unary>)*
<unary>       ::= ("!" | "-" | "+") <expression> | <call>
<primary>     ::= "false" | "true" | "none" | <number> | <string>
      | "self" | <identifier> | "(" <expression> ")"
<number>      ::= <sign>?<nat>("."<nat>)?
<sign>        ::= "-" | "+"
<string>      ::= '"'(.?"\n"?)*'"'
<alpha>       ::= 'a' ... 'z' | 'A' ... 'Z' | '_'
<nat>         ::= <digit><digit>*
<digit>       ::= '0' ... '9'
```
