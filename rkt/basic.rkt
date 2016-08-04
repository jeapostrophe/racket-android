#lang racket/base
(require racket/fixnum
         racket/flonum
         racket/math
         racket/match
         mode-lambda
         lux
         "tablet.rkt"
         #;"tablet-on-desktop.rkt"
         "pixel-c.rkt")

(define-app ([W PIXEL-W] [H PIXEL-H])
  (define fish-idx (sprite-idx csd 'fish))
  (define fish-m 4.0)
  (define fish-x (fl/ (fx->fl W) 2.0))
  (define fish-y (fl/ (fx->fl H) 2.0))
  (struct app
    (spin?)
    #:methods gen:word
    [(define (word-fps w) 30.0)
     (define (word-label w ft)
       (lux-standard-label "App" ft))
     (define (word-output w)
       (define cur-theta
         (if (app-spin? w)
             (fl* (fl/
                   (fx->fl
                    (fxmodulo (fxquotient
                               (current-milliseconds)
                               10)
                              360))
                   360.0)
                  (fl* 2.0 pi))
             0.0))
       (render
        (vector (layer (fx->fl (/ W 2)) (fx->fl (/ H 2)))
                #f #f #f #f #f #f #f)
        '()
        (sprite fish-x fish-y fish-idx
                #:mx fish-m
                #:my fish-m
                #:theta cur-theta)))
     (define (word-event w e)
       (match e
         [(vector 'click x y)
          (cond
            [(fl< (fldist x fish-x y fish-y)
                  (fl* fish-m
                       (flmax (fx->fl (sprite-height csd fish-idx))
                              (fx->fl (sprite-width csd fish-idx)))))
             (play-sound! #"sample.m4a")
             (struct-copy app w
                          [spin? (not (app-spin? w))])]
            [else
             (printf "click ignored: ~v\n" e)
             w])]
         [_
          (printf "event ignored: ~v\n" e)
          w]))
     (define (word-tick w)
       w)])
  (app #f))
