# Control Flow

## If Expressions
If expressions will only evaluate and resolve to their body if their condition is true. The condition must be a `bool`, and does not have to be in parentheses (though because a binary operator expression is used below, which do require parentheses in all positions, it looks like it does)

```rs
main {
    let x: int = 20;

    if (x > 10) {
        out("Greater than 10!"); // condition is met, so this is printed
    }

    if (x > 50) {
        out("Greater than 50!"); // condition is not met, so this is not printed
    }

    out("Done!"); // this always prints
}
```

## Else Expressions
```rs
main {
    let x: int = 5;

    if (x > 10) {
        out("Greater than 10!");
    } else {
        out("Less than (or equal to) 10!"); // condition is not met, so the `else` statement executes and prints
    }

    out("Done!"); // this always prints
}
```

## Else If Expressions
```rs
main {
    let x: int = 10;

    if (x > 10) {
        out("Greater than 10!");
    } else if (x == 10) {
        out("Equal to 10!"); // condition is met, so this `else if` statement executes and prints
    } else {
        out("Less than 10!");
    }

    out("Done!"); // this always prints
}
```

## While Expressions
While expressions will loop and evaluate their body as long as their condition is true. Their condition is rechecked to be true after each loop. The condition must be a `bool`, and does not have to be in parentheses (though because a binary operator expression is used below, it looks like it does)

```rs
main {
    let i: int = 10;
    while (i > 0) {
        out(i);
        i = (i - 1);
    }

    // this program will output: `10 9 8 7 6 5 4 3 2 1`
}
```

## Match Expressions
Match expressions allow for exhaustively evaluating a certain case based on the current variant of an enum value. Since they must be exhaustive, unlike if expressions, there must be a case inside of the switch for each enum variant that the value could be; Omitting any will cause the code to no longer compile.

```rs
enum PlayerState { Playing | Paused | Stopped }

main {
    let state: PlayerState = #Paused;

    // will print "The video is paused!"
    match state {
        #Playing -> out("The video is playing!")
        #Paused -> out("The video is paused!")
        #Stopped -> {
            out("The video is stopped!");
            out("Please select a new video to watch.");
        }
    }
}
```

## Control Flow as Expressions
All control flow items in Steplo are expressions. This means they can be evaluated and used in the position of any other expression, e.g. variable assignment, operands, or function arguments.

```rs
enum PlayerState { Playing | Paused | Stopped }

main {
    let parity: str = if ((5 % 2) == 0) { "even" } else { "odd" };

    let state: PlayerState = #Paused;
    let message: str = match state {
        #Playing -> "The video is playing!"
        #Paused -> "The video is paused!"
        #Stopped -> {
            out("Please select a new video to watch.");
            "The video is stopped!"
        }
    };

    out(message);
}
```
