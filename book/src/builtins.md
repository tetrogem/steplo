# Built-In Functions

The following functions are automatically added to and available in every Steplo program:

## `func out(value: any)`
Prints the given value to `stdout`

## `func in(return: &val)`
Prompts the user for text input, and pauses the program's execution until it's received

## `random_num(return: &num, min: num, max: num)`
Returns a random `num` between `min` and `max` (inclusive). The returned number may always generate with decimals, even if both `min` and `max` are integers.

## `random_int(return: &int, min: int, max: int)`
Returns a random `int` between `min` and `max` (inclusive)

## `random_uint(return: &uint, min: uint, max: uint)`
Returns a random `uint` between `min` and `max` (inclusive)
