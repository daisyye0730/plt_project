# DINOSAUR

> A collaborative team project.

A compiler for a statically-typed, C-like programming language called **Dinosaur** (`.dino`), written in OCaml and targeting LLVM IR. The language supports primitive types, first-class fixed-size lists, control flow with `elif` chains, and user-defined functions — all with full static type checking before code generation.

## Language Features

- **Types:** `int`, `float`, `bool`, `char`, `string`, `List(type, size)`, `None`
- **Operators:** arithmetic, comparison, logical (`&&`, `||`, `not`), unary `++`/`--`
- **Control flow:** `if` / `elif` / `else`, `while`, `for`, `break`, `continue`
- **Lists:** fixed-size typed arrays with indexing (`list[i]`), slicing (`list[a:b]`), and element assignment
- **Functions:** user-defined functions with typed parameters and return types
- **Built-ins:** `print`, `printf`, `prints`, `printb`, `printc`, `min`, `max`, string operations

## Example

```c
int main(){
    List(int, 100) fib;
    int i;

    fib[0] = 0;
    fib[1] = 1;

    for(i=2; i<100; i++){
        fib[i] = fib[i - 1] + fib[i - 2];
    }

    for(i=0; i<10; i++){
        print(fib[i]);
    }

    return 0;
}
```

## Compiler Architecture

The compiler is a classic multi-pass pipeline:

```
Source (.dino)
    ↓
[scanner.mll]   — ocamllex tokenizer
    ↓
[parser.mly]    — ocamlyacc grammar → AST (ast.ml)
    ↓
[semant.ml]     — type checking → SAST (sast.ml)
    ↓
[irgen.ml]      — LLVM IR code generation
    ↓
LLVM IR → lli (JIT execution)
```

## Build & Run

**Build the compiler:**
```bash
ocamlbuild -pkgs llvm dinosaur.native
```

**Compile a `.dino` file to LLVM IR:**
```bash
./dinosaur.native -l fib.dino > fib.out
```

**Execute the compiled IR:**
```bash
lli fib.out
```

**Debug flags:**
```bash
./dinosaur.native -a fib.dino   # print AST
./dinosaur.native -s fib.dino   # print SAST (semantically-checked AST)
./dinosaur.native -l fib.dino   # print LLVM IR (default)
```

## Testing

The test suite lives in `tests/` and includes **72 tests** (40 positive, 32 negative). Positive tests verify correct compilation and output; negative tests verify that the semantic checker correctly rejects ill-typed programs.

```bash
python3 testsuite.py
```

## Compiler Files

| File | Description |
|------|-------------|
| `scanner.mll` | ocamllex lexer — tokenizes source into keywords, literals, operators |
| `parser.mly` | ocamlyacc grammar — produces an untyped AST |
| `ast.ml` | AST type definitions |
| `semant.ml` | Semantic checker — validates types, scopes, and arity; produces SAST |
| `sast.ml` | Semantically-checked AST type definitions |
| `irgen.ml` | LLVM IR code generator using OCaml LLVM bindings |
| `dinosaur.ml` | Compiler driver — wires the pipeline together |
