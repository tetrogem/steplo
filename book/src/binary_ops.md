# Binary Operaters

Binary operators are expressions that take two other expressions as input. An example of a binary operator is the add `+` operator:

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
| any | == (equal to) | any | bool |
| any | != (not equal to) | any | bool |
| any | > (greater than) | any | bool |
| any | < (less than) | any | bool |
| any | >= (greater than or equal to) | any | bool |
| any | <= (less than or equal to) | any | bool |
| bool | && (and) | bool | bool |
| bool | \|\| (or) | bool | bool |
| val | ~ (join) | val | val |
