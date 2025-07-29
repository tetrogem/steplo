# Supertypes, Subtypes, & Type Coercion
Types are able to automatically be coerced into their supertypes.
```
main |foo: int, bar: uint| {
    bar = 10;
    foo = bar; // `uint` is a subtype of `int`, so it can be coerced to type `int`
}
```

## Array Types
An array type `A` is a subtype of another array type `B` if:
1. The element type of `A` is a subtype of the element type of `B`
1. The two array types are the same length
```
main |nums: [num; 5], ints: [int; 5]| {
    ints = [1, 2, 3, 4, 5];
    nums = ints;
}
```

## Struct Types
A struct type `A` is a subtype of another struct type `B` if:
1. `A` & `B` share the same number of fields, with the same names, in the same order
2. The type of a given field in `A` is a subtype of that same field's type in `B`
```
main |coords: { x: num, y: num }, strs: { x: val, y: val }| {
    coords = { x: 1.1, y: -2 };
    strs = coords;
}
```

## Reference Types
A reference type `&A` is a subtype of another reference type `&B` **only if** `A` & `B` are isomorphic, meaning `A` is a subtype of `B` and `B` is a subtype of `A`.
