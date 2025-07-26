# Installation & Getting Started

## Requirements
Steplo is built using Rust, so you must have Rust installed to run the compiler.
- [Install Rust](https://www.rust-lang.org/tools/install)


## Installation
``` bash
## Clone the repo
git clone https://github.com/yourname/steplo.git
cd steplo

## (Optional) Create an output folder, or use an already existing folder
## Note: The local output folder named `out` is .gitignored, so it's safe to create/use!
mkdir out

## Compile a Steplo program
cargo run examples/hel.lo out
```

This will generate a .sb3 file in the out/ directory.

You can now open the .sb3 in the Scratch editor and run your Steplo-compiled project!

## Compiler Options
Run the following to see all CLI options:

```bash
cargo run -- -h
```
## Next Steps
The rest of the chapters in the book will walk you through all of Steplo's features!
