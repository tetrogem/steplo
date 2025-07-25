# Types

## Primitive Types

| Name | Size (Cells) | Extends | Description |
| --- | --- | --- | --- |
| any | 1 |  | Any type that fits in a single cell of stack memory (size 1) |
| val | 1 | any | The base primitive type of Steplo, equivalent to strings in other languages |
| num | 1 | val | 64-bit floating-point numbers (equivalent to TypeScript's `number` type or Rust's `f64` type)  |
| int | 1 | num | A 64-bit floating-point signed integer |
| uint | 1 | num | A 64-bit floating-point unsigned integer |
| bool | 1 | val | A boolean value, either true or false |

## Reference Types

All reference types start with `&` and can be reference to any other type. They are all 1 cell in size, no matter the size of the type they are pointing to. They are represented in memory by the memory address of the value they are pointing to.

## Array Types

Array types are denoted with the following syntax: `&[<element>; <length>]`, where `<element>` is the type of each element in the array, and `<length>` is a `uint` denoting how many elements the array will contain. Arrays are all fixed-size, they cannot grow or shrink. All elements in an array must be of the same type. For example, an array of 10 `ints` would be typed as: `[int; 5]`.
