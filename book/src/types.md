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

Sometimes you'll have very long arrays, where typing each individual element to set isn't feasible, especially when you'd likely want to set all of the elements to a common "default" value. Luckily, you can use the spread `...` operator in an array literal to do this for you:
```
main |long_arr: [int; 999], short_arr: [int; 3]| {
    long_arr = [0...]; // sets all elements to 0
    long_arr = [1, 2, 3, 4, 0...]; // sets the first 4 elements, then the rest to 0
    short_arr = [1, 2, 0...]; // works on all array lengths!
    short_arr = [1, 2, 3, 0...]; // a spread element can still be declared even if it'd go unused
}
```

## Struct Types

Struct types are denoted with braces surrounding key/value pairs: `{ <field>: <type> }`, where `<field>` is the name of a struct field, and `<type>` is the type of that field. Unlike arrays, fields in a struct can be of different types. The order and names of struct fields matter as well; If these mismatch between two types, they are not assignable to eachother.

```
main |coords: { x: num, y: num }, user: { id: uint, name: val }| {
    coords = { x: 10, y: -5.5 };
    user = { id: 1234, name: "Steplo" };
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

## Type Aliases
Retyping complex compound types (e.g., arrays, structs) is tedious and error-prone. Instead of retyping an entire type definition each time it is used, a type alias can be declared, allowing for a shorter name to be used in-place of a more complex type:
```
type Link = {
    name: val,
    redirect: val,
};

type Message = {
    created_time: uint,
    sender_name: val,
    likes: uint,
    content: {
        body: val,
        links: [Link; 2],
    },
};

main |msg: Message| {
    msg = {
        created_time: 123456789,
        sender_name: "Steplo",
        likes: 4,
        content: {
            body: "Hello world!",
            links: [
                { name: "GitHub", redirect: "https://github.com/tetrogem/steplo" },
                { name: "Book", redirect: "https://steplo.tetro.dev/" },
            ],
        },
    };
}
```
