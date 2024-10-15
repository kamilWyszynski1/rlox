# rlox

This is my take on wonderful course made by Robert Nystorm - https://craftinginterpreters.com/

## Overview

rlox is simple interpreter of lox language, language created by Robert in course linked above.
It implements every feature from the course and some additional functionalities like break statement,
class' static fields and simple enums + match keyword.

`/examples` directory could be checked to see what language is capable of.

## How to run

rlox supports two modes - interactive and execution from file.

**Interactive** mode could be run simply by running:

```bash
cargo r
```

**Run from file** could be run like that:

```bash
cargo r -- -f examples/enum.lox
```
