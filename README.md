# Chi

A programming language made for scientific computing, simulation, statistical modeling, and data science.


## Todo

- [x] Rewrite AST to use more composable/modular parts
- [x] Rewrite AST pretty printer to not be so ugly
- [ ] Rework AST to use a `Vec<Decl>` for `root` instead of `Vec<Handle<Decl>>`

## Parser Rework

- [ ] Expressions
    - [x] Atoms
    - [ ] Calls
        - [ ] Regular arguments
        - [ ] Named arguments
    - [ ] Prefix
    - [ ] Postfix
    - [ ] Arithmetic
    - [ ] Logical
    - [ ] Comparison
    - [ ] Equality
    - [ ] Assignment
    - [ ] If exprs
    - [ ] Loops
        - [ ] While
        - [ ] For in
        - [ ] Classical for
- [ ] Declarations
    - [ ] Function declarations
    - [ ] Function definitions
    - [ ] Struct definitions
    - [ ] Struct implementations
    - [ ] Trait definitions
    - [ ] Trait implementations
    - [ ] Enum declarations
    - [ ] Enum implementations
    - [ ] Globals and statics