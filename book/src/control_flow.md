# Control Flow

**Note:** All statements must have a semicolon after them in order to have more statements follow them. This includes control flow statements.

## If Statements
If statements will only execute their body as long as their condition is true. The condition must be a `bool`, and does not have to be in parentheses (though because a binary operator expression is used below, it looks like it does)

```
main |x: int| {
    x = 20;

    if (x > 10) {
        out("Greater than 10!"); // condition is met, so this is printed
    };

    if (x > 50) {
        out("Greater than 50!"); // condition is not met, so this is not printed
    };

    out("Done!"); // this always prints
}
```

## Else Statements
```
main |x: int| {
    x = 5;

    if (x > 10) {
        out("Greater than 10!");
    } else {
        out("Less than (or equal to) 10!"); // condition is not met, so the `else` statement executes and prints
    };

    out("Done!"); // this always prints
}
```

## Else If Statements
```
main |x: int| {
    x = 10;

    if (x > 10) {
        out("Greater than 10!");
    } else if (x == 10) {
        out("Equal to 10!"); // condition is met, so this `else if` statement executes and prints
    } else {
        out("Less than 10!");
    };

    out("Done!"); // this always prints
}
```

## While Statements
While statements will loop and execute their body as long as their condition is true. Their condition is rechecked to be true after each loop. The condition must be a `bool`, and does not have to be in parentheses (though because a binary operator expression is used below, it looks like it does)

```
main |i: int| {
    i = 10;
    while (i > 0) {
        out(i);
        i = (i - 1);
    };

    // this program will output: `10 9 8 7 6 5 4 3 2 1`
}
```
