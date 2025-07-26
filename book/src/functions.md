# Functions & References

## Functions

You can declare functions to be able to reuse code throughout your program.

```
main || {
    // prints "Hello world!" 3 times
    hello();
    hello();
    hello();
}

func hello() || {
    out("Hello world!");
}
```

You can also declare arguments for functions that must be passed to the function to call it:

```
main || {
    // prints "Hello Steplo!"
    hello("Steplo");
}

func hello(name: val) || {
    out((("Hello " ~ name) ~ "!"));
}
```

## References

You can also pass references into functions in order to mutate values on other procedures' stacks. References are created with the reference `&` operator. You can then assign to the memory address pointed to by a reference with the dereference `*` operator. This allows you to create functions that return values:

```
main |sum: int| {
    add(&sum, 1, 2);
    out(sum); // prints 3
}

func add(return: &int, a: int, b: int) || {
    *return = (a + b);
}
```

Functions can declare their own stack variables, too. (Remember though, these variables are only valid for the duration of the function's runtime, so don't return references to a function's stack variables to its caller!)

```
main |sum: int| {
    add(&sum, 1, 2);
    out(sum); // prints 3
}

func add(return: &int, a: int, b: int) |sum: int| {
    sum = (a + b);
    *return = sum;
}
```
