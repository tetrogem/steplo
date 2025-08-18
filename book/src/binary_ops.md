# Binary Operaters

Binary operators can create expressions that take two other expressions as input. An example of a binary operator is the add `+` operator:

```
main || {
    out((1 + 2)); // prints 3
}
```

Currently, all binary operator expressions must be wrapped in parentheses to denote the order of evaluation. Operator precedence will be added in the future, allowing for parentheses to be omitted to utilize a more typical default order of operations.

All the currently existing binary operators and the types they evaluate to:
| Left Type | Operator | Right Type | = Evaluated Type |
| --- | --- | --- | --- |
| uint | + (add) | uint | uint |
| int | + (add) | int | int |
| num | + (add) | num | num |
| int | - (subtract) | int | int |
| num | - (subtract) | num | num |
| uint | * (multiply) | uint | uint |
| int | * (multiply) | int | int |
| num | * (multiply) | num | num |
| num | / (divide) | num | num |
| num | % (mod) | num | num |
| bool | && (and) | bool | bool |
| bool | \|\| (or) | bool | bool |
| str | ~ (join) | val | val |

## Comparison Operators
Comparison operators can be used between any two times that are comparable with eachother.
- Strings are comparable with other strings
- Numbers are comparable with other numbers
- Booleans are comparable with other booleans
- Enum variants are comparable with variants of the same enum

Booleans are not able to be compared ordinally. (Greater than / Less than)

| Operator | = Evaluated Type |
| --- | --- |
| == (equal to) | bool |
| != (not equal to) | bool |
| > (greater than) | bool |
| < (less than) | bool |
| >= (greater than or equal to) | bool |
| <= (less than or equal to) | bool |
