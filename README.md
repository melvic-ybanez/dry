# Dry

Dry is a simple dynamically-typed programming language currently being written in Scala.

Examples:
```scala
// functions in Dry starts with the keyword `def`, just like in Python and Scala
def display_fib(from) {
    // this demonstrates Dry's support for nested functions, lambdas, closures,
    // higher-order functions, and currying
    return lambda(to) {
        for (let i = from; i < to; i = i + 1) {
            // types need to be explicitly converted to string via the `str` function
            print(str(fib(i)) + " ");
        }
        println("");
    };
}

// Another way to create a function is to assign a name to a lambda.
let fib = lambda(n) {
    if (n <= 1) return n;

    // Dry supports recursion, both with lambdas and named functions.
    // Note: At the time of this writing, tail-recursions are not supported
    return fib(n - 2) + fib(n - 1);
};

let fib_from_one_to = display_fib(1);   // partial application in action
fib_from_one_to(5);
fib_from_one_to(10);
fib_from_one_to(15);

// more examples
print("hello\n\tworld");    // same as `println("hello"); print("\tworld");`
println(1 + 3 * 9);         // simple arithmetic. prints 28
println(typeof(10 > 9));    // prints `boolean`
println(5 and 7 or true);   // prints 7. truthiness is (almost) similar to Python's
println(4 << 3);            // bitwise left-shift operator. prints 32

```

**Note:** More details will be added here soon.


