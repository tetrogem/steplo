# Compilations Targets

Steplo has two different targets it can compile to: Scratch and TurboWarp.

You can specify which platform you'd like to target with the `--target` flag:

```bash
# compiles targetting Scratch
cargo run helloworld.lo out --target scratch

# compiles targetting TurboWarp
cargo run helloworld.lo out --target turbowarp
```

How a Steplo project is compiled to a .sb3 file will change based on which platform is being targetted. These changes will be to optimize the program to run most optimially on the targetted platform.

A project compiled targetting Scratch will be outputted with a `.s.sb3` extension, and compilations targetting TurboWarp will have the `.t.sb3` extension.

Of course, since Scratch and TurboWarp both use .sb3 files, you can run a `.t.sb3` on Scratch and a `.s.sb3` on TurboWarp, but they will likely have worse performance than running the corresponding extension for that platform.
