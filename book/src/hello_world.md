# Hello World!

Let's start by learning how to create a simple Hello World program!

Start by creating a new file named `helloworld.lo` anywhere on your computer.

In Steplo, all programs need to have a single `main` procedure, located anywhere within the source code. A main procedure can be declared as follows:

```
main || {

}
```

All of the code to be executed when the program starts goes in the body of the main procedure, which is surrounded by braces.
Let's try calling the built-in `out` function to print to stdout:

```
main || {
    out("Hello world!");
}
```

Now we can compile this program! The compiled Scratch program can be written to any directory on your system, specified by the compile command. In this example, we'll use a directory named `out`. To compile, we can then call:
```bash
cargo run helloworld.lo out
```

This will create `helloworld.sb3` inside of `out`, which we can load into Scratch to see our working program!
