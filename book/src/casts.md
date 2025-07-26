# Type Casting & Transmutation

## Casting
Casting is the inverse of type coercion, and must be manually declared and guaranteed by the programmer. For example, if we coerced an `int` to a `val` like so:
```
main |foo: val, bar: int| {
    bar = 10;
    foo = bar; // automatically coerces `int` to `val`
}
```

We cannot automatically do the opposite to get our `int` back from the `val` variable:
```
main |foo: val, bar: int| {
    bar = 10;
    foo = bar;
    bar = foo; // error: `foo` may be a `val` that is not an `int` (e.g, a `num`, `bool`, or string)
}
```

Since in this case, we *know* bar is `10`, we can tell the compiler we are sure the value of `foo` is in fact assignable to an `int` with a typecast. You can typecast with the cast `<type>` operator:
```
main |foo: val, bar: int| {
    bar = 10;
    foo = bar;
    bar = <int>foo; // no error!
}
```

This is dangerous though; In the future, code could be added that breaks this guarentee, which could lead to runtimes errors the compiler won't warn about:
```
main |foo: val, bar: int| {
    bar = 10;
    foo = bar;
    foo = "hello world"!;
    bar = <int>foo; // this isn't actually an int anymore, but the compiler gives no warning!
}
```

## When is Casting Allowed?
You can't cast all types to all other types. You can only cast type `A` to type `B` if the following conditions are met:
1. `B` is a subtype of `A` OR The in-memory representation of `A` is a subtype of `B`
2. `B` contains no reference types

For example: Casting type `val` to `int` is ok, because:
1. ✅ `int` is a subtype of `val`
2. ✅ `val` is not a reference type

Another example: Casting type `&int` to `val` is ok, because:
1. ✅ The in-memory representation of `&int` (which is a `uint`) is a subtype of `val`
2. ✅ `val` is not a reference type

Now for an example of a cast that is not allowed: You cannot cast from a `&any` to a `&int`, because:
1. ❌ `&int` is not a subtype of `&any`
2. ❌ `&int` is a reference type

You also cannot cast from an `any` to a `&int`, because:
1. ✅ `&int` is a subtype of `any`
2. ❌ `&int` is a reference type

This last cast is disallowed as treating an arbitrary address as a `&int` may break the invariants of the type actually located at that memory address. Casts ensure that they only may possible break the resulting expression being casted, not affecting other values in the program. In order to tell the compiler you'd like to consider a reference to be a different type, you'd need to employ transmuting.

## Transmuting

Any type can be transmuted to any other type of the same size. Unlike casting, no additional checks are done to ensure this conversion makes sense. You can transmute with the transmute `<<type>>` operator:

```
main |integer: int, int_ref: &int, float_ref: &num| {
    integer = 10;
    int_ref = &integer;
    float_ref = <<&num>>int_ref;
    *float_ref = 12.34;
    out(integer); // integer is now a float without ever directly assigning a float to it with a cast
}
```
