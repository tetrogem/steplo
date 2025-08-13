# Built-In Functions

The following functions are automatically added to and available in every Steplo program:

## `func out(value: val)`
Prints the given value to stdout.

## `func in(return: &str)`
Prompts the user for text input, and pauses the program's execution until it's received.

## `random_num(return: &num, min: num, max: num)`
Returns a random `num` between `min` and `max` (inclusive). The returned number may always generate with decimals, even if both `min` and `max` are integers.

## `random_int(return: &int, min: int, max: int)`
Returns a random `int` between `min` and `max` (inclusive).

## `random_uint(return: &uint, min: uint, max: uint)`
Returns a random `uint` between `min` and `max` (inclusive).

## `stdout_clear()`
Clears stdout.

## `stdout_read(return: &str, index: uint)`
Reads the value of line number `index` (starting at 0) of stdout.

## `stdout_write(value: val, index: uint)`
Overwrites the value of line number `index` (starting at 0) of stdout with `value`.

## `stdout_len(return: &uint)`
Returns the number of lines currently printed to stdout.

## `wait_s(duration_s: num)`
Pauses execution of the program for approximately `duration_s` seconds. The implementation of this function is subject to change in the future to allow for more accurate waiting durations.

## `timer_s(return: &num)`
Returns the number of seconds since the start of the program.
