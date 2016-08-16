#lang racket/base
(require racket/fixnum
         racket/flonum
         racket/math
         racket/match
         mode-lambda
         lux
         "spriteboard.rkt"
         "pixel-c.rkt"
         
         #;"tablet.rkt"
         "simulator.rkt")

(define-app ([W PIXEL-W] [H PIXEL-H])
  (define (initialize! sb)
    (define (make-a-fish! x y)
      (define fish-spin? #f)
      (spriteboard-new!
       sb
       #:click!
       (λ ()
         (play-sound! #"sample.m4a")
         (cond
           [fish-spin?
            (set! fish-spin? #f)]
           [else
            (set! fish-spin? (current-milliseconds))]))
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
    
    (define N 6)
    (for* ([x (in-range N)]
           [y (in-range N)])
      (make-a-fish! (fl* (fl/ (fl+ 0.5 (fx->fl x)) (fx->fl N)) (fx->fl W))
                    (fl* (fl/ (fl+ 0.5 (fx->fl y)) (fx->fl N)) (fx->fl H)))))
  (make-spriteboard W H csd render initialize!))
