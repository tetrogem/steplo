# Types

## Primitive Types

| Name | Size (Cells) | Extends | Description |
| --- | --- | --- | --- |
| val | 1 | | The base primitive type of Steplo, equivalent to strings in other languages |
| num | 1 | val | 64-bit floating-point numbers (equivalent to TypeScript's `number` type or Rust's `f64` type)  |
| int | 1 | num | A 64-bit floating-point signed integer |
| uint | 1 | int | A 64-bit floating-point unsigned integer |
| bool | 1 | val | A boolean value, either true or false |

```
main |string: val, float: num, integer: int, index: usize, maybe: bool| {
    string = "hello world!";
    float = 12.34;
    integer = -7;
    index = 10;
    maybe = true;
}
```

## Reference Types

All reference types start with `&` and can be reference to any other type. They are all 1 cell in size, no matter the size of the type they are pointing to. They are represented in memory by the memory address of the value they are pointing to. No reference types are subtypes of any other reference type.

```
main |integer: int, int_ref: &int| {
    integer = 10;
    int_ref = &integer;
}
```

## Array Types

Array types are denoted with the following syntax: `[<element>; <length>]`, where `<element>` is the type of each element in the array, and `<length>` is a `uint` denoting how many elements the array will contain. Arrays are all fixed-size, they cannot grow or shrink. All elements in an array must be of the same type. For example, an array of 10 `ints` would be typed as: `[int; 10]`.

```
main |nums: [num; 5]| {
    nums = [1, 2, 3, 4, 5];
}
```

## The `any` Type
Any is a special type that is a supertype of any type that is 1 cell in size. This includes all primitives, references, and arrays with a single 1 cell-sized element.

```
main |anything: any, integer: int| {
    integer = 10;

    anything = "hello!";
    anything = 10;
    anything = true;
    anything = &integer;
}
```

## Type Coercion
Types are able to automatically be coerced into their supertypes.
```
main |foo: int, bar: uint| {
    bar = 10;
    foo = bar; // `uint` is a subtype of `int`, so it can automatically be coerced to type `int`
}
