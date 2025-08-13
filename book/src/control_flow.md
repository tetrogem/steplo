# Control Flow

## If Statements
If statements will only execute their body as long as their condition is true. The condition must be a `bool`, and does not have to be in parentheses (though because a binary operator expression is used below, it looks like it does)

```
main |x: int| {
    x = 20;

    if (x > 10) {
        out("Greater than 10!"); // condition is met, so this is printed
    }

    if (x > 50) {
        out("Greater than 50!"); // condition is not met, so this is not printed
    }

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
    }

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
    }

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
    }

    // this program will output: `10 9 8 7 6 5 4 3 2 1`
}
```

## Match Statements
Match statements allow for exhaustively executing a certain case based on the current variant of an enum value. Since they must be exhaustive, unlike if statements, there must be a case inside of the switch for each enum variant that the value could be; Omitting any will cause the code to no longer compile.

```
enum PlayerState { Playing | Paused | Stopped }

main |state: PlayerState| {
    state = #Paused;

    // will print "The video is paused!"
    match state {
        #Playing -> out("The video is playing!");
        #Paused -> out("The video is paused!");
        #Stopped -> {
            // you can use braces to run multiple statements
            out("The video is stopped!");
            out("Please select a new video to watch.")
        }
    }
}
```
