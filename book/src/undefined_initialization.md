# Undefined Initialization

Steplo usually requires all variables to be initialized on definition, to ensure they allows contain a valid value for their type when used. In some cases though, this value may be expensive to create, and would only be set temporarily to statisfy the compiler. In these cases, you *can* explicitly tell the compiler to instead not initialize the memory of a variable when declared. It is then up to the programmer though to ensure they use the variable safely. Undefined initialization can be done by assigning a variable to `undefined`.

```rs
main {
    let expensive: [arr; 9999] = undefined;
}
```
