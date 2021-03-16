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

;This function rotates a block clock wise, each column of characters from left to right will become a row from top to bottom.
;The rotate-clockwise function works by constructing a list of strings (the rows for the new block), where each string has
;an element from each of the original block’s rows.

(define/contract (rotate-clockwise block)
  (-> valid-block? valid-block?)
  (for/list ([a (in-string (first block))]
             [b (in-string (second block))]
             [c (in-string (third block))]
             [d (in-string (fourth block))])
    (string d c b a)))

; helper function to test clockwise rotation of all blocks
(define/contract (all-rotations block)
  (-> valid-block? (listof valid-block?))
  (reverse
   (for/fold ([rotations (list block)])
             ([n (in-range 4)])
     (cons (rotate-clockwise (car rotations)) rotations))))

;; test code
;(block->pict L-Block)
(map block->pict all-blocks)
;(block->pict (rotate-clockwise L-Block))
(map block->pict (all-rotations L-Block))