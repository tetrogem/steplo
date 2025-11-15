# Variables

## Stack Variables

Next, let's learn how to create variables on the stack. Stack variables are able to have their values modified, as well as their addresses stored as references. Stack variables live for the duration of the procedure they were declared for; That means once the procedure ends, any references to its stack variables will be invalid! Steplo does not currently perform checks for if references are valid for you, so this is something you'll need to keep in mind while programming.

Let's start by declaring a variable on our main procedure:

```rs
main {
    let hello: str = "";
}
```

All stack variables are declared with the `let` keyword as a statement within a procedure. They also must have a type! In this case, we've created a variable `hello` with the type `str`, which is the primitive string type. Variables must also be initialized! You cannot create a variable without specifying its initial value. This ensures the variable can always be read to return a valid instance of its type.

We can then use this value in-place of the literal we printed in the previous chapter, passing it to `out` as a parameter:

```rs
main {
    let hello: str = "Hello world!";
    out(hello);
}
```

This program should have the same output as the previous "Hello World!" program!

You can also reassign a variable to a different value:

```rs
main {
    let hello: str = "Hello world!";
    hello = "Goodbye!";
}
```

You can declare as many stack variables as you want in a single procedure, and they can be declared anywhere within the procedure:

```rs
main {
    let hello: str = "Hello";
    out(hello);

    let world: str = "World!";
    out(world);
}
```

Variables can also be shadowed, where a variable can be declared with the same name as a previous variable:

```rs
main {
    let hello: str = "Hello";
    out(hello);

    let hello: num = 5;
    out((num * 2));
}
```

Shadowing variables do not need to have the same type as the previous variable with the same name, and it is not the same as reassigning a variable. In practice, you are creating a brand new variable, which only shares its name with the previous one. The shadowed variable is no longer accessible after shadowing, as its name now points to the new variable.

Variables are also block scoped:

```rs
main {
    let hello: str = "Hello"; // `hello` exists in this block and all blocks within it

    {
        let world: str = "World!"; // `world` exists in this block and all blocks within it
        out(hello);
        out(world);
    }

    out(hello);
    out(world); // Error! `world` is not in scope
}
```

## Static Variables

Variables can also be declared as a static item in the top-level of a program, with the `static` keyword:

```rs
static GLOBAL: int = 0;

main {}
```

These variables are global, and can be accessed by any procedure within a program without being directly passed into them as arguments. One limitation with static variables though is they must be initialized with a constant value (e.g. a literal). Using other expressions, such as function calls or control flow items, are not currently supported. Though, static variables can later be re-assigned to any valid expression during program execution:

```rs
static GLOBAL: int = 0;

main {
    GLOBAL = double(2);
}

fn double(x: int) -> int (x * 2)
```
