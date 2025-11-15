# Type Casting & Transmutation

## Casting
You can cast a type `A` to another type `B` if they both have the same underlying representation, and the cast would not break any invariants of type `A` or type `B`.

Unlike type coercion, casts must be explicitly declared with the typecast `<type>` operator.

The underlying representation of a type is how it is stored in memory as primitive types. For example, an array of two `uint`s `[uint; 2]` and a struct containing only two `uint` fields `{ foo: uint, bar: uint }` would both be stored in memory as 2 cells, both containing `uints`. Because of this, they can be casted between each other:
```rs
main {
    let arr: [uint; 2] = [123, 456];
    let str:  { foo: uint, bar: uint } = <{ foo: uint, bar: uint }>arr;
    arr = <[uint; 2]>str;
}
```

There are some types that you cannot cast to or from though, even if they have the same underlying representations. Most notably, this includes reference types. This is because casting would break the invariant of a reference being a memory address to a *specific* type (note: the programmer is still responsible for upholding that the reference is alive and hasn't been freed before use). As all references are underlyingly represented by `uint`s, casting would allow for a reference to any type to be casted to a reference of any other type, which is not safe behavior.

```rs
main {
    let x: num = 10;
    let x_ref: &num = &x;

    // this is not allowed!
    let str_ref: &str = <&str>num_ref;
}
```

Instead, if you must convert between reference types, you would need to perform a **transmutation**.

## Transmuting

Any type can be transmuted to any other type of the same size. Unlike casting, no additional checks are done to ensure this conversion makes sense or is safe. You can transmute with the transmute `<<type>>` operator:

```rs
main {
    let x: int = 10;
    let x_ref: &int = &x;

    let y_ref: &num = <<&num>>x_ref;
    *y_ref = 12.34;

    out(x); // `x` is now no longer an integer, without ever directly assigning a non-integer to it with a transmutation
}
```
