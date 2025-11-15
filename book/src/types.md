# Types

## Primitive Types

| Name | Size (Cells) | Extends | Description |
| --- | --- | --- | --- |
| val | 1 | | The base primitive type of Steplo |
| bool | 1 | val | A boolean value, either true or false |
| str | 1 | val | A string value, representing text  |
| num | 1 | val | 64-bit floating-point numbers (equivalent to TypeScript's `number` type or Rust's `f64` type)  |
| int | 1 | num | A 64-bit floating-point signed integer |
| uint | 1 | int | A 64-bit floating-point unsigned integer |

```rs
main {
    let string: str = "hello world!";
    let float: num = 12.34;
    let integer: int = -7;
    let index: uint = 10;
    let maybe: bool = true;
}
```

## Enum Types

Functionally, enums work similar to primitive types, except they are user-defined. Each enum is given a set of variants that are applicable to its type. For example, an enum for the state of a video player may be represented as:
```rs
enum PlayerState { Playing | Paused | Stopped }
```

You can assign to variables of an enum type with variant literals, which consist of the enum name, a # (tag symbol), and then the name of the variant:

```rs
main {
    let state: PlayerState = PlayerState#Playing;
    state = PlayerState#Paused;
    state = PlayerState#Stopped;
}
```

In some cases where the type can be inferred by the compiler, the enum name can be omitted from the variant literal:

```rs
main {
    // state is a PlayerState
    // so each variant literal below must also be a PlayerState
    let state: PlayerState = #Playing;
    state = #Paused;
    state = #Stopped;
}
```

Enums also have an inherent order, to allow for use with the greater than (`>`) and less than (`<`) operators. The order of the variants will be determined by the order they were declared on the enum declaration. So in the previous `PlayerState` example, `#Playing < #Paused < #Stopped` would be true. All enums are 1 cell in size, as they are internally represented by a `uint`.

## Reference Types

All reference types start with `&` and can be reference to any other type. They are all 1 cell in size, no matter the size of the type they are pointing to. They are represented in memory by the memory address of the value they are pointing to. No reference types are subtypes of any other reference type.

```rs
main {
    let integer: int = 10;
    let int_ref: &int = &integer;
}
```

## Array Types

Array types are denoted with the following syntax: `[<element>; <length>]`, where `<element>` is the type of each element in the array, and `<length>` is a `uint` denoting how many elements the array will contain. Arrays are all fixed-size, they cannot grow or shrink. All elements in an array must be of the same type. For example, an array of 10 `ints` would be typed as: `[int; 10]`.

```rs
main {
    let nums: [num; 5] = [1, 2, 3, 4, 5];
}
```

Sometimes you'll have very long arrays, where typing each individual element to set isn't feasible, especially when you'd likely want to set all of the elements to a common "default" value. Luckily, you can use the spread `...` operator in an array literal to do this for you:
```rs
main {
    let long_arr: [int; 999] = [0...]; // sets all elements to 0
    long_arr = [1, 2, 3, 4, 0...]; // sets the first 4 elements, then the rest to 0

    let short_arr: [int; 3] = [1, 2, 0...]; // works on all array lengths!
    short_arr = [1, 2, 3, 0...]; // a spread element can still be declared even if it'd go unused
}
```

## Struct Types

Struct types are denoted with braces surrounding key/value pairs: `{ <field>: <type> }`, where `<field>` is the name of a struct field, and `<type>` is the type of that field. Unlike arrays, fields in a struct can be of different types. The order and names of struct fields matter as well; If these mismatch between two types, they are not assignable to eachother.

```rs
main {
    coords: { x: num, y: num } = { x: 10, y: -5.5 };
    user: { id: uint, name: str } = { id: 1234, name: "Steplo" };
}
```

## The `any` Type
Any is a special type that is a supertype of any type that is 1 cell in size. This includes all primitives, references, and arrays with a single 1 cell-sized element.

```rs
main {
    let integer: int = 10;

    let anything: any = "hello!";
    anything = 10;
    anything = true;
    anything = &integer;
}
```

## Type Aliases
Retyping complex compound types (e.g., arrays, structs) is tedious and error-prone. Instead of retyping an entire type definition each time it is used, a type alias can be declared, allowing for a shorter name to be used in-place of a more complex type:
```rs
type Link = {
    name: str,
    redirect: str,
};

type Message = {
    created_time: uint,
    sender_name: str,
    likes: uint,
    content: {
        body: str,
        links: [Link; 2],
    },
};

main {
    let msg: Message = {
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
