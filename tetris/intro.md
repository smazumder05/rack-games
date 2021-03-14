# Tetris

```racket
#lang racket/gui
(require pict racket/draw racket/contract)
(module+ test (require rackunit)
```
The [contract system](https://docs.racket-lang.org/reference/contracts.html) guards one part of a program from another. We specify the behavior of a module’s exports via (provide (contract-out ....)), and the contract system enforces those constraints.


