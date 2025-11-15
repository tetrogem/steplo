# Functions & References

## Functions

You can declare functions to be able to reuse code throughout your program.

```rs
main {
    // prints "Hello world!" 3 times
    hello();
    hello();
    hello();
}

fn hello() {
    out("Hello world!");
}
```

You can also declare arguments for functions that must be passed to the function to call it:

```rs
main {
    // prints "Hello Steplo!"
    hello("Steplo");
}

fn hello(name: val) {
    out((("Hello " ~ name) ~ "!"));
}
```

Functions evaluate and return their body expression. If they do not return the unit struct `{}`, their return types must be defined with `->`:
```rs
main {
    let doubled = double(5); // 10
    let rem = remainder(6, 4); // 2
}

fn double(x: num) -> num (x * 2)

fn remainder(numer: num, denom: num) -> num {
    let quotient_floor = num_floor((numer / denom));
    (numer - (quotient_floor * denom))
}
```

## References

You can also pass references into functions in order to mutate values on other procedures' stacks. References are created with the reference `&` operator. You can then assign to the memory address pointed to by a reference with the dereference `*` operator. This allows you to create functions that return values:

```rs
main {
    add(&sum, 1, 2);
    out(sum); // prints 3
}

fn add(result: &int, a: int, b: int) {
    *result = (a + b);
}
```

Functions can declare their own stack variables, too. (Remember though, these variables are only valid for the duration of the function's runtime, so don't return references to a function's stack variables to its caller!)

```rs
main {
    let sum: int = 0;
    add(&sum, 1, 2);
    out(sum); // prints 3
}

fn add(result: &int, a: int, b: int) {
    let sum = (a + b);
    *result = sum;
}
```
