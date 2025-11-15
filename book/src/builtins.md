# Built-In Functions

The following items are automatically added to and available in every Steplo program.

## Public

These items are APIs intended to be used by developers in their applications.

### `enum Key { Space | UpArrow | DownArrow | RightArrow | LeftArrow | A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | X | Y | Z | Num0 | Num1 | Num2 | Num3 | Num4 | Num5 | Num6 | Num7 | Num8 | Num9 }`
Represents a specific detectable keyboard key.

### `type KeyEvent = { key: Key, time: num }`
Represents a key event sent by the user. `key` is the key interacted with, and `time` is the time the key was interacted with in seconds since 00:00:00 1 January 2000 (UTC).

### `fn out(value: val)`
Prints the given value to stdout.

### `fn in() -> str`
Prompts the user for text input, and pauses the program's execution until it's received. Returns the text inputted by the user.

### `fn num_random(min: num, max: num) -> num`
Returns a random `num` between `min` and `max` (inclusive). The returned number may always generate with decimals, even if both `min` and `max` are integers.

### `fn int_random(min: int, max: int) -> int`
Returns a random `int` between `min` and `max` (inclusive).

### `fn uint_random(min: uint, max: uint) -> uint`
Returns a random `uint` between `min` and `max` (inclusive).

### `fn uint_round(x: uint) -> uint`
Rounds the given `uint`, while maintaining the invariant that it is unsigned. I honestly don't know why I implemented this function though, the number you pass in will already be an integer.

### `fn num_round(x: num) -> int`
Rounds the given `num`, safely converting it to an `int`.

### `fn uint_ceil(x: uint) -> uint`
Returns the ceiling of the given `uint`, while maintaining the invariant that it is unsigned. These `uint` rounding functions may be removed in a future release unless someone can actually find a use for them.

### `fn num_ceil(x: num) -> int`
Returns the ceiling of the given `num`, safely converting it to an `int`.

### `fn int_abs(x: int) -> uint`
Returns the absolute value of the given `int`, while maintaining the invariant that it is an integer. As the absolute value is always positive, this function can safely return a `uint`.

### `fn num_abs(x: num) -> int`
Returns the absolute value of the given `num`, safely converting it to an `int`.

### `fn stdout_clear()`
Clears stdout.

### `fn stdout_read(index: uint) -> str`
Reads and returns the value of line number `index` (starting at 0) of stdout.

### `fn stdout_write(value: val, index: uint)`
Overwrites the value of line number `index` (starting at 0) of stdout with `value`.

### `fn stdout_len() -> uint`
Returns the number of lines currently printed to stdout.

### `fn key_events_len() -> uint`
Returns the length of the Key Events queue (how many unpolled key events exist).

### `fn key_events_has_next() -> bool`
Returns `true` if the Key Events queue is not empty.

### `fn key_events_next() -> KeyEvent`
Pops and returns the next `KeyEvent` from the Key Events queue.

### `fn wait_s(duration_s: num)`
Pauses execution of the program for approximately `duration_s` seconds. The implementation of this function is subject to change in the future to allow for more accurate waiting durations.

### `fn timer_s() -> num`
Returns the number of seconds since the start of the program.

### `fn since_2000_days() -> num`
Returns the number of days since 00:00:00 1 January 2000 (UTC).

## Internal

These items are used to implement Public APIs and are intended for internal use by the compiler only. They are publicly accessible in Steplo programs, but are not recommended for use (as they could break invariants held by Public APIs if not used properly)

### `fn key_events_key_queue_clear()`
Clears the Key Events' Key Queue. Not clearing the Key Events' Time Queue at the same time will cause them to possibly go out of sync. See `key_events_len`, `key_events_has_next`, and `key_events_next` for the Public version of this API.

### `fn key_events_key_queue_delete(index: uint)`
Delete the element at index `index` in the Key Events' Key Queue. Not deleting the same element `index` in the Key Events' Time Queue at the same time will cause them to possibly go out of sync. See `key_events_len`, `key_events_has_next`, and `key_events_next` for the Public version of this API.

### `fn key_events_key_queue_read(index: uint) -> Key`
Returns the element at index `index` in the Key Events' Key Queue. Not deleting the same element `index` in the Key Events' Time Queue at the same time will cause them to possibly go out of sync. See `key_events_len`, `key_events_has_next`, and `key_events_next` for the Public version of this API.

### `fn key_events_key_queue_len() -> uint`
Returns the length of the Key Events' Key Queue. See `key_events_len`, `key_events_has_next`, and `key_events_next` for the Public version of this API.

### `fn key_events_time_queue_clear()`
Clears the Key Events' Time Queue. Not clearing the Key Events' Key Queue at the same time will cause them to possibly go out of sync. See `key_events_len`, `key_events_has_next`, and `key_events_next` for the Public version of this API.

### `fn key_events_time_queue_delete(index: uint)`
Delete the element at index `index` in the Key Events' Time Queue. Not deleting the same element `index` in the Key Events' Key Queue at the same time will cause them to possibly go out of sync. See `key_events_len`, `key_events_has_next`, and `key_events_next` for the Public version of this API.

### `fn key_events_time_queue_read(index: uint) -> num`
Return the element at index `index` in the Key Events' Time Queue. The returned value will be a time in seconds since 00:00:00 1 January 2000 (UTC). Not deleting the same element `index` in the Key Events' Key Queue at the same time will cause them to possibly go out of sync. See `key_events_len`, `key_events_has_next`, and `key_events_next` for the Public version of this API.

### `fn key_events_time_queue_len() -> uint`
Returns the length of the Key Events' Time Queue. See `key_events_len`, `key_events_has_next`, and `key_events_next` for the Public version of this API.
