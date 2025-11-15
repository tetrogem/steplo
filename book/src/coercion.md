# Supertypes, Subtypes, & Type Coercion
Types are able to automatically be coerced into their supertypes.
```rs
main {
    let bar: uint = 10;
    let foo: int = bar; // `uint` is a subtype of `int`, so it can be coerced to type `int`
}
```

## Array Types
An array type `A` is a subtype of another array type `B` if:
1. The element type of `A` is a subtype of the element type of `B`
1. The two array types are the same length
```rs
main {
    let ints: [int; 5] = [1, 2, 3, 4, 5];
    let nums: [num; 5] = ints;
}
```

## Struct Types
A struct type `A` is a subtype of another struct type `B` if:
1. `A` & `B` share the same number of fields, with the same names, in the same order
2. The type of a given field in `A` is a subtype of that same field's type in `B`
```rs
main {
    let coords: { x: num, y: num } = { x: 1.1, y: -2 };
    let vals: { x: val, y: val } = coords;
}
```

## Reference Types
A reference type `&A` is a subtype of another reference type `&B` **only if** `A` & `B` are isomorphic, meaning `A` is a subtype of `B` and `B` is a subtype of `A`.
