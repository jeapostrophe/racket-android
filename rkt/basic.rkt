#lang racket/base
(require racket/fixnum
         racket/flonum
         racket/math
         racket/match
         mode-lambda
         lux
         "spriteboard.rkt"
         "pixel-c.rkt"

         ;; Require this to build on the tablet
         #;"tablet.rkt"
         ;; Require this to use the simulator
         "simulator.rkt")

(define-app ([W PIXEL-W] [H PIXEL-H])
  (define (initialize! sb)
    (define (make-a-fish! x y)
      (define fish-spin? #f)
      (spriteboard-clickable!
       sb
       #:click!
       (λ ()
         (play-sound! #"sample.m4a")
         (cond
           [fish-spin?
            (set! fish-spin? #f)]
           [else
            (set! fish-spin? (current-milliseconds))]))
       #:drag-drop!
       (λ (v)
         (printf "Chomp chomp! ~v\n" v))
       #:sprite
       (λ ()
         (sprite x y
                 (sprite-idx csd 'fish)
                 #:m 4.0
                 #:theta
                 (if fish-spin?
                     (fl* (fl/
                           (fx->fl
                            (fxmodulo (fxquotient
                                       (- (current-milliseconds) fish-spin?)
                                       10)
                                      360))
                           360.0)
                          (fl* 2.0 pi))
                     0.0)))))
    (define (make-a-dragger! i x y)
      (define dropped? #f)
      (define idx (sprite-idx csd i))
      (spriteboard-draggable!
       sb
       #:init-x x
       #:init-y y
       #:sprite
       (λ (dragging? x y)
         (sprite x y idx
                 #:m 4.0))
       #:drag-start!
       (λ () (printf "Drag start!\n"))
       #:drag-stop!
       (λ ()
         (set! dropped? #t)
         (printf "Drag stop!\n"))
       #:drag-drop-v
       (λ () i)
       #:drag-drop!
       (λ (v) (printf "Chomp chump ~v ~v\n" i v))
       #:alive?
       (λ () (not dropped?))))
    (define (make-a-cloud! x y)
      (make-a-dragger! 'cloud x y))
    (define (make-a-jack! x y)
      (make-a-dragger! 'jack x y))

    (define N 6)
    (for* ([x (in-range N)]
           [y (in-range N)])
      (define maker!
        (cond
          [(and (= x 0) (= y 0))
           make-a-cloud!]
          [(and (= x 3) (= y 3))
           make-a-jack!]
          [else
           make-a-fish!]))
      (maker! (fl* (fl/ (fl+ 0.5 (fx->fl x)) (fx->fl N)) (fx->fl W))
              (fl* (fl/ (fl+ 0.5 (fx->fl y)) (fx->fl N)) (fx->fl H)))))
  (make-spriteboard W H csd render initialize!))
