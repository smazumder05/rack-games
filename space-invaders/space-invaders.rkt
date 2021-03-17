;; A space invaders game in Racket

#lang racket/gui
(require pict racket/draw racket/contract)

;; Return true if ROW is a valid block row, which means that it is a string
;; containing only the Z character or spaces.
(define (valid-block-row? row)
  (and (string? row)                     ; a row is a string
       (for/and ([item (in-string row)]) ; containing only valid characters
         (and (member item '(#\space #\. #\Z)) #t))))

;; Return true if BLOCK is a valid block, meaning that it is a list containing
;; rows which pass the VALID-BLOCK-ROW? test.
(define (valid-block? block)
  (and (list? block)                      ; a block is a list
       (andmap valid-block-row? block)))  ; ... each element is a valid row

;; Size of a block square in pixels, all sprites are built out of squares this
;; size.
(define square-size 4)

;; Produce a pict from a string containing a single line from a sprite frame
;; definition.  The resulting pict will use the default color, and the color
;; of the final pict can be changed using "colorize"
(define/contract (row->squares row)
  (-> string? pict?)
  (define items
    (for/list ([char (in-string row)])
      (if (equal? char #\Z)
          (filled-rectangle square-size square-size)
          (ghost (rectangle square-size square-size)))))
  (apply hc-append items))

;; Produce a PICT corresponding to a sprite frame.  This allows specifying the
;; color to use for the resulting pict.
(define/contract (block->pict block #:color (color "steelblue"))
  (->* (valid-block?) (#:color (or/c string? (is-a?/c color%))) pict?)
  (colorize (apply vc-append (map row->squares block)) color))


;;...................................................... animation-snip% ....


;; Snip class for out animation snip.  This is required for creating new
;; snips, see animation-snip% below.
(define animation-snip-class
  (make-object
   (class snip-class%
     (super-new)
     (send this set-classname "animation-snip-class"))))

;; An animation snip will show a sequence of snips, updating them at defined
;; intervals.  The snip will be displayed in the DrRacket REPL making it
;; useful for visualizing the animated sprites that we define.
;;
;; This is not used in the game, but can be used when designing the animation
;; for the sprites.  See also the `animate` function below.
;;
;; The object is initialized with two parameters: PICTS is a list of pict
;; objects, INTERVAL is the refresh interval, defaulting to 500ms.
(define animation-snip%
  (class snip%
    (init-field picts [interval 500])
    (super-new)
    (send this set-snipclass animation-snip-class)

    ;; Determine the width and height of the snip itself, such that it will
    ;; fit all the picts we have, even if they are different sizes.
    (define width (apply max (map pict-width picts)))
    (define height (apply max (map pict-height picts)))

    ;; The index of the current pict being displayed
    (define index 0)

    ;; Advance to the next picture in the list and tell the snip admin that
    ;; this snip needs to be re-displayed.  This is called from the refresh
    ;; timer.
    (define (on-refresh)
      (set! index (modulo (add1 index) (length picts)))
      (define admin (send this get-admin))
      (when admin
        (send admin needs-update this 0 0 width height)))

    ;; This is the timer which will call on-refresh periodically, to implement
    ;; the animation.
    (define timer (new timer% [interval interval] [notify-callback on-refresh]))

    ;; Implement a copy method for our snip -- this is needed by DrRacket REPL
    ;; which copies snips internally.
    (define/override (copy)
      (new animation-snip% [picts picts] [interval interval]))

    ;; Return the size (extend) of this snip -- the pasteboard in DrRacket
    ;; calls this method to know how much space to allocate for the snip.
    (define/override (get-extent dc x y w h descent space lspace rspace)
      (when w (set-box! w width))
      (when h (set-box! h height))
      ;; NOTE: technically, for picts we can compute these as well
      (when descent (set-box! descent 0.0))
      (when space (set-box! space 0.0))
      (when lspace (set-box! lspace 0.0))
      (when rspace (set-box! rspace 0.0)))

    ;; This is the method which draws the snip -- it is invoked by the
    ;; DrRacket pasteboard, when the snip should be displayed.
    (define/override (draw dc x y . other)
      (define pict (list-ref picts index))
      (define ox (* (- width (pict-width pict)) 0.5))
      (define oy (* (- height (pict-height pict)) 0.5))
      (draw-pict pict dc (+ x ox) (+ y oy)))
    ))

;; Construct an animation snip from a list of pict objects.  If this function
;; is called in the DrRacket REPL, the animation will be shown directly.
;;
;; This is just a wrapper around the animation-snip% construction, but easier
;; to type.
(define (animate picts #:interval [interval 500])
  (new animation-snip% [picts picts] [interval interval]))


;;............................................................... sprite ....

;; A sprite defines an animation for a game character.  The animation is a
;; sequence of bitmaps plus a frame time (which indicates how often the frames
;; are updates).  The sprite also holds the width and height of the animation,
;; plus the color, which is used to create other sprites of the same color
;; (e.g. an explosion from a space ship sprite).
(struct sprite (bitmaps
                frame-time
                width
                height
                color) #:transparent)

;; Return the number of frames in the sprite S.
(define (sprite-frame-count s)
  (length (sprite-bitmaps s)))

;; Create a sprite from a sequence of pictures, PICTS and a COLOR.  The color
;; is only stored in the sprite.
(define (make-sprite picts color #:refresh-interal (frame-time 500))
  (define width (apply max (map pict-width picts)))
  (define height (apply max (map pict-height picts)))
  (define bitmaps
    (for/list ([p (in-list picts)])
      (pict->bitmap p)))
  (sprite bitmaps frame-time width height color))


;;....................................................... game over pict ....

;; Picts for the game over overlay, one for winning the game, other for
;; loosing it.

(define game-over-pict/win
  (let* ([label (text "Game Over: You Win" (cons 'bold 'default) 48)]
         [background (filled-rounded-rectangle
                      (+ 25 (pict-width label))
                      (+ 25 (pict-height label)))])
    (cc-superimpose
     (cellophane (colorize background '(221 221 221)) 0.9)
     (colorize label '(165 0 38)))))

(define game-over-pict/lose
  (let* ([label (text "Game Over: You Lose" (cons 'bold 'default) 48)]
         [background (filled-rounded-rectangle
                      (+ 25 (pict-width label))
                      (+ 25 (pict-height label)))])
    (cc-superimpose
     (cellophane (colorize background '(221 221 221)) 0.9)
     (colorize label '(165 0 38)))))



;;............................................................ Explosion ....

;; The explosion sprite is a bit special, as it is constructed several times,
;; one for each space invader color.

(define explosion-frame-a
  '("              "
    "              "
    "     ZZZZ     "
    "   ZZZZZZZZ   "
    "     ZZZZ     "
    "              "
    "              "))

(define explosion-frame-b
  '("              "
    "  Z  Z  Z  Z  "
    "   Z  ZZ  Z   "
    " ZZ Z    Z ZZ "
    "   Z  ZZ  Z   "
    "  Z  Z  Z  Z  "
    "              "))

(define explosion-frame-c
  '(" Z  Z    Z  Z "
    "  Z  Z  Z  Z  "
    "   Z      Z   "
    "ZZ          ZZ"
    "   Z      Z   "
    "  Z  Z  Z  Z  "
    " Z  Z    Z  Z "))

(define explosion-frame-d
  '(" Z  Z    Z  Z "
    "  Z        Z  "
    "              "
    "Z            Z"
    "              "
    "  Z        Z  "
    " Z  Z    Z  Z "))

(define explosion-frame-e
  '(" Z          Z "
    "              "
    "              "
    "              "
    "              "
    "              "
    " Z          Z "))

(define explosion-frame-f
  '("              "
    "              "
    "              "
    "              "
    "              "
    "              "
    "              "))

(define explosion-animation
  (for/list ([z (list explosion-frame-a explosion-frame-b
                      explosion-frame-c explosion-frame-d
                      explosion-frame-e explosion-frame-f)])
    (block->pict z)))

;; Create an explosion sprite of the specified COLOR (which is taken from one
;; for the alien sprites when it is destroyed.
(define (make-explosion-sprite color)
  (define picts
    (for/list ([z (list explosion-frame-a explosion-frame-b
                        explosion-frame-c explosion-frame-d
                        explosion-frame-e explosion-frame-f)])
      (block->pict z #:color color)))
  (make-sprite picts color #:refresh-interal 100))



;;............................................................... Zabrak ....

;; Wikipedia has pages for alien names:
;;
;; https://en.wikipedia.org/wiki/List_of_fictional_alien_species:_Z

(define zabrak-color "Steel Blue")

(define zabrak-frame-a
  '("   ZZZ   "
    " ZZZZZZZ "
    "ZZZZZZZZZ"
    "Z  ZZZ  Z"
    "ZZZZZZZZZ"
    "  Z   Z  "
    " Z ZZZ Z "
    "Z       Z"))

(define zabrak-pict-a (block->pict zabrak-frame-a #:color zabrak-color))

(define zabrak-frame-b
  '("   ZZZ   "
    " ZZZZZZZ "
    "ZZZZZZZZZ"
    "Z  ZZZ  Z"
    "ZZZZZZZZZ"
    "  Z   Z  "
    " Z ZZZ Z "
    " Z     Z "))

(define zabrak-pict-b (block->pict zabrak-frame-b #:color zabrak-color))

(define zabrak-frame-c
  '("   ZZZ   "
    " ZZZZZZZ "
    "ZZZZZZZZZ"
    "Z  ZZZ  Z"
    "ZZZZZZZZZ"
    "  Z   Z  "
    " Z ZZZ Z "
    "  Z   Z  "))

(define zabrak-pict-c (block->pict zabrak-frame-c #:color zabrak-color))

(define zabrak-animation
  (list zabrak-pict-a zabrak-pict-b zabrak-pict-c zabrak-pict-b))

(define zabrak (make-sprite zabrak-animation zabrak-color))



;;.............................................................. Zakdorn ....

(define zakdorn-color "Dark Magenta")

(define zakdorn-frame-a
  '("   ZZZ   "
    " ZZZZZZZ "
    "ZZZZZZZZZ"
    "Z  ZZZ  Z"
    "ZZZZZZZZZ"
    " Z ZZZ Z "
    " Z     Z "
    "  ZZZZZ  "))

(define zakdorn-pict-a (block->pict zakdorn-frame-a #:color zakdorn-color))

(define zakdorn-frame-b
  '("   ZZZ   "
    " ZZZZZZZ "
    "ZZZZZZZZZ"
    "Z  ZZZ  Z"

    "ZZZZZZZZZ"
    " Z ZZZ Z "
    " Z    Z  "
    " ZZZZZ   "))

(define zakdorn-pict-b (block->pict zakdorn-frame-b #:color zakdorn-color))

(define zakdorn-frame-c
  '("   ZZZ   "
    " ZZZZZZZ "
    "ZZZZZZZZZ"
    "Z  ZZZ  Z"
    "ZZZZZZZZZ"
    " Z ZZZ Z "
    "  Z    Z "
    "   ZZZZZ "))

(define zakdorn-pict-c (block->pict zakdorn-frame-c #:color zakdorn-color))

(define zakdorn-animation
  (list zakdorn-pict-a zakdorn-pict-b zakdorn-pict-a zakdorn-pict-c))

(define zakdorn (make-sprite zakdorn-animation zakdorn-color))


;;............................................................... Zaldan ....

(define zaldan-color "Dark Olive Green")

(define zaldan-frame-a
  '("  Z    Z  "
    "   Z  Z   "
    "  ZZZZZZ  "
    " ZZ ZZ ZZ "
    "ZZZZZZZZZZ"
    "Z ZZZZZZ Z"
    "   Z  Z   "
    "  Z    Z  "))

(define zaldan-pict-a (block->pict zaldan-frame-a #:color zaldan-color))

(define zaldan-frame-b
  '("   Z  Z   "
    "   Z  Z   "
    "  ZZZZZZ  "
    " ZZ ZZ ZZ "
    "ZZZZZZZZZZ"
    "Z ZZZZZZ Z"
    "   Z  Z   "
    "  Z    Z  "))

(define zaldan-pict-b (block->pict zaldan-frame-b #:color zaldan-color))

(define zaldan-frame-c
  '("    ZZ    "
    "   Z  Z   "
    "  ZZZZZZ  "
    " ZZ ZZ ZZ "
    "ZZZZZZZZZZ"
    "Z ZZZZZZ Z"
    "   Z  Z   "
    "  Z    Z  "))

(define zaldan-pict-c (block->pict zaldan-frame-c #:color zaldan-color))

(define zaldan-animation
  (list zaldan-pict-a zaldan-pict-b zaldan-pict-c zaldan-pict-b))

(define zaldan (make-sprite zaldan-animation zaldan-color))


;;............................................................... Zalkon ....

(define zalkon-color "Firebrick")

(define zalkon-frame-a
  '("  Z  Z  "
    "   ZZ   "
    "Z ZZZZ Z"
    "ZZ ZZ ZZ"
    "ZZZZZZZZ"
    "Z ZZZZ Z"
    "Z  ZZ  Z"
    "ZZ    ZZ"))

(define zalkon-pict-a (block->pict zalkon-frame-a #:color zalkon-color))

(define zalkon-frame-b
  '("  Z  Z  "
    "Z  ZZ  Z"
    "Z ZZZZ Z"
    "ZZ ZZ ZZ"
    "ZZZZZZZZ"
    "Z ZZZZ Z"
    "ZZ ZZ ZZ"
    "       "))

(define zalkon-pict-b (block->pict zalkon-frame-b #:color zalkon-color))

(define zalkon-frame-c
  '("Z Z  Z Z"
    "Z  ZZ  Z"
    "Z ZZZZ Z"
    "ZZ ZZ ZZ"
    "ZZZZZZZZ"
    "ZZZZZZZZ"
    "        "
    "        "))

(define zalkon-pict-c (block->pict zalkon-frame-c #:color zalkon-color))

(define zalkon-animation
  (list zalkon-pict-a zalkon-pict-b zalkon-pict-c zalkon-pict-b))

(define zalkon (make-sprite zalkon-animation zalkon-color))


;;................................................................ Zarbi ....

(define zarbi-color "Saddle Brown")

(define zarbi-frame-a
  '("    Z    "
    "   ZZZ   "
    "  ZZZZZ  "
    " ZZZZZZZ "
    "ZZ  Z  ZZ"
    "ZZZZZZZZZ"
    "  ZZZZZ  "
    " Z  Z  Z "
    "Z  Z Z  Z"))

(define zarbi-pict-a (block->pict zarbi-frame-a #:color zarbi-color))

(define zarbi-frame-b
  '("    Z    "
    "   ZZZ   "
    "  ZZZZZ  "
    " ZZZZZZZ "
    "ZZ  Z  ZZ"
    "ZZZZZZZZZ"
    "  ZZZZZ  "
    " Z ZZZ Z "
    " Z     Z "))

(define zarbi-pict-b (block->pict zarbi-frame-b #:color zarbi-color))

(define zarbi-frame-c
  '("    Z    "
    "   ZZZ   "
    "  ZZZZZ  "
    " ZZZZZZZ "
    "ZZ  Z  ZZ"
    "ZZZZZZZZZ"
    "  ZZZZZ  "
    " Z ZZZ Z "
    "  Z   Z  "))

(define zarbi-pict-c (block->pict zarbi-frame-c #:color zarbi-color))

(define zarbi-animation
  (list zarbi-pict-a zarbi-pict-b zarbi-pict-c zarbi-pict-b))

(define zarbi (make-sprite zarbi-animation zarbi-color))

#;(map animate (list zabrak-animation
                     zakdorn-animation
                     zaldan-animation
                     zalkon-animation
                     zarbi-animation
                     explosion-animation))


;;............................................................. the-arena ....

;; The arena is where the game takes place, it is the central game loop as
;; well as the GUI interface...

;; A list are all the objects managed by the arena
(define scene '())

;; Add an object to the arena.  The object will receive the canvas size
;; immediately by calling `canvas-size-changed`.
(define (add-actor actor)
  (set! scene (cons actor scene)))

;; Remove an object from the arena
(define (remove-actor actor)
  (set! scene (remove actor scene)))

;; The outcome of the game: 'undecided means the game is in progress,
;; 'abandoned is used when the user closes the window, while 'win and 'lose
;; are self explanatory.
(define game-outcome 'undecided)

;; The toplevel window of an application is an instance of `frame%`, but,
;; since we need to intercept key presses (to move the cannon and shoot the
;; laser), plus know when the window is closed, we create a subclass of frame%
;; and instantiate that.  We override `on-close` and `on-subwindow-char` to
;; pass on the information every actor.
(define the-frame
  (new (class frame%
         (super-new [label "Space Invaders"] [width 800] [height 600])
         (define/augride (on-close)
           (set! game-outcome 'abandoned))
         (define/override (on-subwindow-char receiver event)
           (for ([o (in-list scene)])
             (send o keyboard-event event))
           (super on-subwindow-char receiver event)))))

;; Called when the canvas is redrawn -- we call the paint method on all the
;; arena objects, as each object is supposed to know how to paint itself.  If
;; the game is won or lost, the appropriate "game over" overlay is also
;; displayed.
(define (on-canvas-paint canvas dc)
  (for ([o (in-list scene)])
    (send o paint canvas dc))
  (when (member game-outcome '(win lose))
    (define pict (if (equal? game-outcome 'win)
                     game-over-pict/win
                     game-over-pict/lose))
    (let-values ([(width height) (send dc get-size)])
      (let ([x (/ (- width (pict-width pict)) 2)]
            [y (/ (- height (pict-height pict)) 2)])
        (draw-pict pict dc x y)))))

;; The game will be drawn on a `canvas%`, but we also want to know when the
;; size of the canvas change (to adjust the position of the elements of the
;; game), so we create a subclass of canvas, overriding `on-size` and
;; instantiate that one.
(define the-canvas
  (new (class canvas%
         (super-new [parent the-frame] [paint-callback on-canvas-paint])
         (define/override (on-size _width _height)
           ;; the width and height passed to `on-size` are that of the entire
           ;; window, which is not useful for us.  Instead, we call
           ;; `get-client-size` which returns the size of the drawing area,
           ;; and pass this information to `size-changed-callback`
           (define-values (w h) (send this get-client-size))
           (for ([o (in-list scene)])
             (send o canvas-size-changed w h))))))

;; The game loop: each game frame it calls update for every arena object with
;; the time it passed, requests a canvas refresh, than sleeps for the
;; remaining frame time, repeating the process.
(define (run-game-loop #:frame-rate [frame-rate 60])
  (set! game-outcome 'undecided)
  (send the-frame show #t)
  (send the-frame focus)

  ;; While FPS is something most users can understand (e.g. 60 FPS, 30FPS),
  ;; what a game really cares about is the amount of time that each game frame
  ;; should take, which is the inverse of the frame rate.  We also convert the
  ;; value to milliseconds, as this is what our timer uses.
  (define frame-time (* (/ 1.0 frame-rate) 1000.0))

  ;; Mark the timestamp for the start of the game, this is used to compute a
  ;; "game time" for the update method.
  (define start-timestamp (current-inexact-milliseconds))

  (let loop ()
    (define now (current-inexact-milliseconds))
    (define game-time (- now start-timestamp))
    (for ([o (in-list scene)])
      (send o update game-time))
    (send the-canvas refresh)
    (define update-duration (- (current-inexact-milliseconds) now))
    (define remaining-time (- frame-time update-duration))
    (sleep/yield (/ (max 0 remaining-time) 1000.0))
    (unless (equal? game-outcome 'abandoned)
      (loop))))


;........................................................ actor% ....

;; This is the interface that the arena% expects from all objects it manages.
;; It defines the methods that the arena% itself invokes on each object it
;; manages.
(define actor<%>
  (interface ()
    [keyboard-event (->m (is-a?/c key-event%) any/c)]
    [canvas-size-changed (->m number? number? any/c)]
    [paint (->m (is-a?/c canvas%) (is-a?/c dc<%>) any/c)]
    [update (->m positive? any/c)]))

;; ... all objects managed by the arena need only to provide the methods that
;; are invoked by the arena%, however it is useful to provide a general
;; implementation for arena objects, keeping all the common functionality in
;; the same place.  Most objects will not be interested in keyboard events, so
;; they will provide an empty `keyboard-event` method implementation, might as
;; well do that here too...
;;
;; See the implementation for what else is provided by this common actor%
;; class.
;;
(define actor%
  (class* object% (actor<%>)

    ;; Each actor will display a bitmap at position X, Y (which is in the
    ;; center of the bitmap).  We provide these as fields, so derived classes
    ;; can access and set them, while this class will provide an
    ;; implementation of `paint` (derived classes may still provide their own
    ;; paint method, if they need to).
    (init-field [bitmap #f]
                [x 0]
                [y 0])

    ;; Creation time is the game time when the object was created (initialized
    ;; on the first call to `update`, while `last-time` is the last time the
    ;; `update` was called and it is used to calculate delta times.  Normally,
    ;; derived objects don't need to access these values (although they are
    ;; available), they should instead override `update/life-time` or
    ;; `update/delta-time`, or maybe `update` itself.
    (field [creation-time #f]
           [last-time #f])

    (super-new)

    ;; Show this invader on the canvas.  If we have a bitmap, we display it at
    ;; the (x, y) coordinates (these coordinates are assumed to be in the
    ;; center of the bitmap)
    (define/public (paint canvas dc)
      (when bitmap
        (define width (send bitmap get-width))
        (define height (send bitmap get-height))
        (define top-left-x (- x (/ width 2)))
        (define top-left-y (- y (/ height 2)))
        (send dc draw-bitmap bitmap top-left-x top-left-y)))

    ;; Implement a more useful update method.  The arena% will pass a "game
    ;; time" which is a time in milliseconds since an arbitrary start point
    ;; (when the arena object was created).  Most objects will be interested
    ;; in either the "delta time" -- amount of time since last call to update,
    ;; or "life time" -- amount of time since the object itself was created.
    ;; This update method will keep track of both and call the
    ;; "update/life-time" and "update/delta-time" methods which child classes
    ;; might want to override (they can override this method as well).
    (define/public (update game-time)
      (if creation-time
          (update/life-time (- game-time creation-time))
          (set! creation-time game-time))
      (when last-time
        (update/delta-time (- game-time last-time)))
      (set! last-time game-time))

    ;; These methods are Implemented as "empty methods" which do nothing, they
    ;; are here so the arena can call them, but they don't do anything.  Child
    ;; objects might want to override them if they want to handle the
    ;; respective events.
    (define/public (keyboard-event e) (void))
    (define/public (canvas-size-changed w h) (void))
    (define/public (update/life-time lifetime) (void))
    (define/public (update/delta-time dt) (void))

    ))



;;.............................................................. cannon% ....

(define cannon-color "Dark Slate Gray")

(define cannon-frame-a
  '("      ZZ      "
    "  ZZZZZZZZZZ  "
    "  ZZZZZZZZZZ  "
    "ZZZZZZZZZZZZZZ"))

(define cannon-pict (block->pict cannon-frame-a #:color cannon-color))

(define cannon%
  (class actor%
    (init-field [speed 1e-1])
    (super-new
     [bitmap (pict->bitmap cannon-pict)])
    (inherit-field bitmap x y)

    (define direction 0)                ; -1 left, 1 right
    (define-values (left-limit right-limit) (values 0 100))
    (define shoot? #f)
    (define first-time? #t)

    (define/override (keyboard-event event)
      (case (send event get-key-code)
        ((release)
         (when (member (send event get-key-release-code) '(left right))
           (set! direction 0)))
        ((left) (set! direction -1))
        ((right) (set! direction 1))
        ((#\space) (set! shoot? #t))))

    (define/override (canvas-size-changed new-width new-height)
      (define width (send bitmap get-width))
      (define height (send bitmap get-height))
      (set! left-limit (/ width 2))
      (set! right-limit (- new-width (/ width 2)))
      (set! y (- new-height (/ height 2)))
      (set! x (max left-limit (min right-limit x)))
      (when (and first-time? (> right-limit left-limit))
        (set! x (/ (- right-limit left-limit) 2))
        (set! first-time? #f))
      )
      
    (define/override (update/delta-time dt)
      (define distance (* direction speed dt))
      (set! x (max left-limit (min right-limit (+ x distance))))
      (when shoot?
        (set! shoot? #f)              ; reset it
        (define height (send bitmap get-height))
        (add-actor (new laser-shot% [x x] [y (- y height)]))))

    ))


;;.......................................................... laser-shot% ....

(define laser-shot-frame
  '("Z"
    "Z"
    "Z"
    "Z"))

(define laser-shot-pict (block->pict laser-shot-frame #:color cannon-color))

(define laser-shot%
  (class actor%
    (init-field [speed 5e-1])
    (super-new
     [bitmap (pict->bitmap laser-shot-pict)])
    (inherit-field bitmap x y)

    (define/override (update/delta-time dt)
      (define distance (* speed dt))
      (set! y (- y distance))
      (if (< y 0)
          ;; The laser shot has left the arena
          (remove-actor this)
          (let ((tip-x x)
                (tip-y (- y (/ (send bitmap get-height) 2))))
            (for/or ([o (in-list scene)]
                     #:when (is-a? o space-ship%))
              (define-values (left right top bottom) (send o get-bounding-box))
              (define hit? (and (> x left) (< x right) (> y top) (< y bottom)))
              (when hit?
                (send o destroy)
                (remove-actor this))
              hit?))))

    ))


;;................................................................ fleet ....

(define fleet%
  (class actor%
    (init-field [movement-speed 5e-2])
    (super-new)

    (define x 0)
    (define y 0)

    (define ships '())
    (define movement-direction 'left)
    (define left-movement-limit 0)
    (define right-movement-limit 100)
    (define down-movement-limit 1000)
    (define drop-height 100)            ; arbitrary
    (define drop-limit 100)             ; arbitrary

    (define/public (get-coordinates)
      (values x y))

    (define/public (set-drop-height h)
      (set! drop-height h))

    (define/override (canvas-size-changed new-width new-height)
      (set! left-movement-limit 0)
      (set! right-movement-limit new-width)
      (set! down-movement-limit new-height))

    (define/override (update/delta-time dt)
      (define distance (* movement-speed dt))
      (case movement-direction
        ((left) (set! x (- x distance)))
        ((right) (set! x (+ x distance)))
        ((down) (set! y (+ y distance))))

      (maybe-change-direction))

    (define/public (add-ship ship)
      (send ship set-the-fleet this)
      (set! ships (cons ship ships)))

    (define/public (remove-ship ship)
      (set! ships (remove ship ships))
      (send ship set-the-fleet #f)
      (when (null? ships)
        (set! game-outcome 'win)))

    (define/private (get-bounding-box)
      (for/fold ([left #f] [right #f] [top #f] [bottom #f])
                ([ship (in-list ships)])
        (define-values (l r t b) (send ship get-bounding-box))
        (values (if left (min left l) l)
                (if right (max right r) r)
                (if top (min top t) t)
                (if bottom (max bottom b) b))))

    (define/private (maybe-change-direction)
      (define-values (left right top bottom) (get-bounding-box))
      (case movement-direction
        ((left)
         (when (and left (< left left-movement-limit))
           (set! movement-direction 'down)
           (set! drop-limit (+ top drop-height))))
        ((right)
         (when (and right (> right right-movement-limit))
           (set! movement-direction 'down)
           (set! drop-limit (+ top drop-height))))
        ((down)
         (if (and bottom (> bottom down-movement-limit))
             ;; Invasion successful
             (begin
               (set! movement-speed 0)
               (set! game-outcome 'lose))
             (when (and top (> top drop-limit))
               (set! movement-direction (if (and left (< left 0)) 'right 'left)))))))

    ))

(define fleet-member<%>
  (interface ()
    [set-the-fleet (->m (or/c (is-a?/c fleet%) #f) any/c)]
    [get-bounding-box (->m (values real? real? real? real?))]))


;;.......................................................... space-ship% ....

(define space-ship%
  (class* actor% (fleet-member<%>)
    (init-field sprite pos-x pos-y [cycle? #t])
    (super-new)

    (field [the-fleet #f])

    (define/public (set-the-fleet f)
      (set! the-fleet f))

    (define/public (get-bounding-box)
      (define top-left-x (- pos-x (/ (sprite-width sprite) 2)))
      (define top-left-y (- pos-y (/ (sprite-height sprite) 2)))
      (define-values (dx dy)
        (if the-fleet
            (send the-fleet get-coordinates)
            (values 0 0)))
      (values (+ dx top-left-x)
              (+ dx top-left-x (sprite-width sprite))
              (+ dy top-left-y)
              (+ dy top-left-y (sprite-height sprite))))

    (define frame-index 0)

    (define/override (update/life-time life-time)
      (define frame-time (sprite-frame-time sprite))
      (define frame-count (sprite-frame-count sprite))
      (define steps (exact-truncate (/ life-time frame-time)))
      (set! frame-index (modulo steps frame-count)))

    (inherit-field bitmap x y)
    
    (define/override (paint canvas dc)
      (define-values (dx dy)
        (if the-fleet
            (send the-fleet get-coordinates)
            (values 0 0)))
      (set! x (+ pos-x dx))
      (set! y (+ pos-y dy))
      (set! bitmap (list-ref (sprite-bitmaps sprite) frame-index))
      (super paint canvas dc))

    (define/public (destroy)
      (define explosion (new explosion%
                             [color (sprite-color sprite)]
                             [pos-x pos-x]
                             [pos-y pos-y]))
      (add-actor explosion)
      (remove-actor this)
      (send the-fleet add-ship explosion)
      (send the-fleet remove-ship this))

    ))

(define (create-space-ships
         fleet
         [invaders (list zabrak zakdorn zaldan zalkon zarbi)]
         [columns 11])

  ;; Find the width and height of each alien ship, the final cell size will be
  ;; the maximum one, so all align nicely.
  (define width (apply max (map sprite-width invaders)))
  (define height (apply max (map sprite-height invaders)))
  (define spacing (max (* 1/5 width) (* 1/5 height)))

  ;; Note that the coordinates of of the space ship is in the center of the
  ;; image!
  (for* ([(sprite row) (in-indexed (in-list invaders))]
         [column (in-range columns)])
    (define space-ship
      (new space-ship%
           [sprite sprite]
           [pos-x (+ (* column (+ width spacing)) (* 1/2 width))]
           [pos-y (+ (* row (+ height spacing)) (* 1/2 height))]))
    (add-actor space-ship)
    (send fleet add-ship space-ship))

  (send fleet set-drop-height height))


;;............................................................ explosion ....

;; The explosion is an animation that replaces a space ship when it is hit by
;; a laser shot.  Unlike space ships, when the animation frames are completed,
;; the explosion removes itself from the scene.
(define explosion%
  (class* actor% (fleet-member<%>)
    (init-field color pos-x pos-y)
    (super-new)
    (field [the-fleet #f])

    (define/public (set-the-fleet f)
      (set! the-fleet f))
    
    (define sprite (make-explosion-sprite color))

    (define/public (get-bounding-box)
      (define top-left-x (- pos-x (/ (sprite-width sprite) 2)))
      (define top-left-y (- pos-y (/ (sprite-height sprite) 2)))
      (define-values (dx dy)
        (if the-fleet
            (send the-fleet get-coordinates)
            (values 0 0)))
      (values (+ dx top-left-x)
              (+ dx top-left-x (sprite-width sprite))
              (+ dy top-left-y)
              (+ dy top-left-y (sprite-height sprite))))

    (define frame-index 0)

    (define/override (update/life-time life-time)
      (define frame-time (sprite-frame-time sprite))
      (set! frame-index (exact-truncate (/ life-time frame-time)))
      (when (>= frame-index (sprite-frame-count sprite))
        (send the-fleet remove-ship this)
        (remove-actor this)))
    
    (inherit-field bitmap x y)
          
    (define/override (paint canvas dc)
      (define-values (dx dy)
        (if the-fleet
            (send the-fleet get-coordinates)
            (values 0 0)))
      (set! x (+ pos-x dx))
      (set! y (+ pos-y dy))
      (when (< frame-index (sprite-frame-count sprite))
        (set! bitmap (list-ref (sprite-bitmaps sprite) frame-index)))
      (super paint canvas dc))

    ))


;;................................................................. main ....

(module+ main
  (add-actor (new cannon%))
  (define fleet (new fleet%))
  (add-actor fleet)
  (create-space-ships fleet)
  (run-game-loop))

