# Dry

Dry is a simple dynamically-typed programming language currently being written in Scala.

The grammar, in BNF, will be provided here soon, but the syntax should be familiar
to Python and Scala developers:

<img width="875" alt="Screen Shot 2022-11-06 at 11 16 42 PM" src="https://user-images.githubusercontent.com/4519785/200179103-f6b7b544-75ae-47ea-b429-3d25f3427ae6.png">

### Contents
1. [Installation](#installation)
1. Getting Started
1. [Running the tests](#running-the-tests)
1. [Examples](#examples)
1. Grammar

# Installation
Installation instructions will be provided after the first release.

# Running the tests
The project has a custom sbt command for running the test:
```
$ sbt testDry
```
If everything works correctly, your console should print a bunch of assertion results. The end of the logs should look like this:
```
[Success] Updated first element
[Success] Size is the number of elements passed to the `list` function
[Success] For all list `xs` and integer `i`, `xs.at(i)` is equal to `xs._<i>`
[Success] Index out of bounds
[Success] List type
[Success] New element is added to the list
[Success] Size should increase after adding new item
[Success] Access variable from imported module
[Success] Access function from imported module
[Success] Modify values from imported module
[Success] Init should save the set variables
[Success] Init should be capable of accepting parameters
[Success] Type of class
[Success] Type of instance
[Success] Ducks should quack!
[Success] Denji should say 'Woof!'
[Success] Class properties should be updated
Ran 33 tests. Successful: 33. Failed: 0.
```
The tests themselves are written in Dry. You can see the directory containing them here: https://github.com/melvic-ybanez/dry/tree/main/tests. All the files in that directory that start with `test_` and have the Dry extension will be picked up by the `testDry` command.

# Examples

#### Basic functional programming:
```
// a function in Dry starts with the keyword `def`, just like in Python and Scala
def display_fib(from) {
  // this demonstrates Dry's support for nested functions, lambdas, closures,
  // higher-order functions, and currying
  return lambda(to) {
    for (let i = from; i <= to; i = i + 1) {
      // types need to be explicitly converted to string via the `str` function
      // before we can use it as an operand of the string concatenation operator
      print(str(fib(i)) + " ");
    }
    println("");
  };
}

// Another way to create a named function is to assign a name to a lambda.
let fib = lambda(n) {
  if (n <= 1) return n;

  // Dry supports recursion, both with lambdas and named functions.
  // Note: At the time of this writing, tail-call optimizations are not supported
  return fib(n - 2) + fib(n - 1);
};

let fib_from_one_to = display_fib(1);   // partial application in action
fib_from_one_to(5);         // 1 1 2 3 5
fib_from_one_to(10);        // 1 1 2 3 5 8 13 21 34 55
fib_from_one_to(15);        // 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610

display_fib(1)(5);          // 1 1 2 3 5
```

#### Classes:
```
class Fib {
  def compute() {
    for (let i = self.start; i <= self.end; i = i + 1) {
      print(str(fib(i)) + " ");   // convert a number to string before concatenation, like in Python
    }
    println("");
  }

  def from(start) { self.start = start; return self; }

  def to(end) { self.end = end; return self; }
}

Fib().from(1).to(5).compute();  // 1 1 2 3 5

class Pet {
  // default implementation
  def make_sound() {
    println("quack!");
  }
}

let duck = Pet();
duck.make_sound();       // prints "quack!"

let denji = Pet();
// a custom sound for Denji
denji.make_sound = lambda() {
    println("Woof");
};
denji.make_sound();     // prints "Woof"
```

#### More examples
```
print("hello\n\tworld");    // same as `println("hello"); print("\tworld");`
println(1 + 3 * 9);         // simple arithmetic. prints 28
println(typeof(10 > 9));    // prints `boolean`
println(typeof("hello"));   // prints `string`
println(5 and 7 or true);   // prints 7. truthiness is (almost) similar to Python's
println(4 << 3);            // bitwise left-shift operator. prints 32
```

**Note:** More details will be added here soon.
