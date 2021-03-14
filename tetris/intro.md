# Tetris

```racket
#lang racket/gui
(require pict racket/draw racket/contract)
(module+ test (require rackunit)
```
The [contract system](https://docs.racket-lang.org/reference/contracts.html) guards one part of a program from another. We specify the behavior of a module’s exports via (provide (contract-out ....)), and the contract system enforces those constraints.

### 1. Step 1 - define the tetris blocks

- There are 7 shapes in tetris,all of them fit into a 4x4 grid. The blocks will be using letters which roughly resemble their shape (from left to right): I, Q (from square), L, J, T, Z and S. 

- Each block can be defined as a list of strings, with the dot (.) character stating for an empty square and a letter standing for a filled in one
