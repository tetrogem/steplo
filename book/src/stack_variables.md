# Stack Variables

Next, let's learn how to create variables on the stack. Stack variables are able to have their values modified, as well as their addresses stored as references. Stack variables live for the duration of the procedure they were declared for; That means one the procedure ends, any references to its stack variables will be invalid! Steplo does not currently perform checks for if references are valid for you, so this is something you'll need to keep in mind while programming.

Let's start by declaring a variable on our main procedure:

```
main |hello: str| {

}
```

All stack variables are declared in the two pipes `| |` at the start of a procedure. They also must have a type! In this case, we've created a variable `hello` with the type `str`, which is the primitive string type.

Now, let's assign a value to our variable:

```
main |hello: str| {
    hello = "Hello world!";
}
```

We can then use this value in-place of the literal we printed in the previous chapter, passing it to `out` as a parameter:

```
main |hello: str| {
    hello = "Hello world!";
    out(hello);
}
```

This program should have the same output as the previous "Hello World!" program!

**Note:** Variables are not initialized with a value automatically; If you use a variable before setting it yourself, the exact value it will contain is undefined.

You can also declare multiple variables in a single procedure:

```
main |hello: str, world: str| {
    hello = "Hello";
    world = "World!";
    out(hello);
    out(world);
}
```
