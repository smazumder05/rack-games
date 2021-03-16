#lang racket/gui
(require pict racket/draw racket/contract)
(module+ test (require rackunit))

; define the 7 blocks as strings 
(define I-Block
  '(".I.."
   ".I.."
   ".I.."
   ".I.."))
(define Q-Block
  '("...."
   ".QQ."
   ".QQ."
   "...."))
(define T-Block
  '(".T.."
   "TTT."
   "...."
   "...."))
(define Z-Block
  '("ZZ.."
   "ZZ.."
   "Z..."
   "...."))
(define L-Block
  '("LL.."
   ".L.."
   ".L.."
   "...."))
(define S-Block
  '("S..."
   "SS.."
   ".S.."
   "...."))
(define J-Block
  '(".JJ."
   ".J.."
   ".J.."
   "...."))

(define all-blocks (list I-Block  J-Block  L-Block  S-Block  Q-Block  T-Block Z-Block  ))

; Functions to check if the blocks are valid
(define (valid-block-row? row)
  (and (string? row)
       (= (string-length row) 4)
       (for/and ([item (in-string row)])
           (and (member item '(#\. #\I #\Q #\L #\J #\T #\Z #\S)) #t))))

(define (valid-block? block)
  (and (list? block)
       (= (length block) 4)
       (andmap valid-block-row? block)))

(module+ test
    (check-false (valid-block-row? 1))
    (check-false (valid-block-row? "......."))
    (check-false (valid-block-row? "X..."))

    (check-false (valid-block-row? "world"))
    (check-false (valid-block-row? (append L-Block T-Block S-Block)))
    (check-false (valid-block-row? (list "...." "...." "...." )))
    (check-false (valid-block-row? (list "X..." "...." "...." "....")))

    ;; Verify that all blocks are correctly defined
    (for ([block (in-list all-blocks)])
         (check-pred valid-block? block)))

;; Define a 4X4 grid with colored squares filled in for each block
(define square-size 30)

(define colors
  (hash
   #\I (make-color 0 118 187)
   #\J (make-color 238 119 51)
   #\L (make-color 0 153 36)
   #\Q (make-color 51 187 238)
   #\S (make-color 136 34 85)
   #\T (make-color 204 51 17)
   #\Z (make-color 238 51 119)))

(define/contract (block->pict block)
  (-> valid-block? pict?)
  (apply vc-append (map row->squares block)))

(define/contract (row->squares row)
  (-> string? pict?)
  (define items
    (for/list ([char (in-string row)])
      (define color (hash-ref colors char #f))
      (if color
          (filled-rectangle square-size square-size #:color color)
          (rectangle square-size square-size))))
  (apply hc-append items))

(block->pict Z-Block)
(map block->pict all-blocks)
