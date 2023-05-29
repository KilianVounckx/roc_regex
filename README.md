# Regex

A toy regex implementation in [roc](https://www.roc-lang.org/).

It is far from feature complete, and probably doesn't follow any spec. I am just working on this
to learn about regex, roc and language theory.

## What is there

* Regex objects can be parsed from strings as in most programming languages.
* `matches` function which checks if a string matches a regex object (does not return the substring)

### Supported regex syntax

* choices (with `|`)
* choice groups (with `[]`)
* grouped regex expressions (with `()`)
* star operator (`*`)
* plus operator (`+`)
* question mark operator (`?`)

## Plans

* choice group negation (with `^` in beginning of `[]`)
* range expressions (with `-` in `[]`)
* metacharacters (like `.`, `\d`, `\w`, ...)
* position anchors (like `^` and `$`, `\b`, `\A` and `\Z`, ...)

## Non-plans

Some things I explicitly don't want to get into at the moment. This doesn't mean they won't ever
get implemented, but it means it is not something I am interested in right now.

* Unicode support (for now only ascii. when not combined with operators, unicode should work)
* State machine (at the moment the implementation uses regex derivatives. to get a more performant
  engine, you would need to convert it to a state machine)
* High performance (This is a toy engine so I can learn)
