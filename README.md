# clike

An implementation of a C-like language in Haskell.

The language compiles to bytecode and can be run in the
VM provided in this project.

## Features

- `int`, `byte`, and `ptr` types
- Strings, pointers, memory operations
- module imports

## Usage

This project is build with Stack.

Compile the file with the `-c` flag, and run the generated
bytecode with the `-e` flag.

```
stack run -- -c file.cl
stack run -- -e bc
```

## Examples

```
import stdlib;

func fac(int x) : int {
  if (x <= 1) {
    return 1;
  }

  return x * facrec(x - 1);
}

func main() : void {
  int f;
  str message;

  f = fac(10);
  message = "the factorial of 10 is ";

  printStr(message);
  printInt(f);
  printNewline();
}
```
