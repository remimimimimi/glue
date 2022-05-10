# Bozon

A new programming language designed to be flexible, bring together the best features of
the academics and the engineering worlds.

## Features

- Easy to use and performant (by user choice) memory model.
- Ultimate package manager that can handle bozon packages, as well as projects
  in other languages.
- JIT compilation, compilation, and interpretation for the same language to match different use cases.
- Universal syntax with powerful macroses.
- Flexible semantic, that can be defined by users.

## Running

Install rust via your package manager or [rustup](https://rustup.rs/), then run following command

``` sh
cargo run --release
```

## Roadmap

- [x] General AST
- [x] Parser
- [X] Query system
- [ ] Protolang
  + [ ] AST
  + [ ] Parser
  + [ ] Interpreter
  + [ ] Macroses
  + [ ] Support for editors
  + [ ] Compilation via LLVM
- [ ] Dependently typed main language
- [ ] Package manager

## Thanks

- To [Magmide](https://github.com/magmide/magmide) for the formulation of the thoughts that I had.
- To [Racket](https://github.com/racket/racket) for macroses.
- To [Rust](https://github.com/rust-lang/rust) for a beautiful ecosystem and friendly community.
- To [Cone](https://github.com/jondgoodwin/cone) for promising memory management model.
- To other smaller but not less important projects.
