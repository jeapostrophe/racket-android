#lang racket/base
(require racket/fixnum
         racket/flonum
         racket/math
         racket/match
         mode-lambda
         mode-lambda/text/runtime
         lux
         "spriteboard.rkt"
         "devices.rkt"

         ;; Require this to build on the tablet
         #;"tablet.rkt"
         ;; Require this to use the simulator
         "tabulator.rkt")

(define-app ([W (* 2 PIXEL-W)] [H (* 2 PIXEL-H)])
  (define-static-font the-font "csd-font.rktd")
  (define text-render (make-text-renderer the-font csd))

  (define (initialize! sb)
    (define (make-a-std-fish! x y m action)
      (define fish-spin?-box (box #f))
      (spriteboard-clickable!
       sb
       #:click!
       (λ () (action fish-spin?-box))
       #:drag-drop!
       (λ (v)
         (printf "Chomp chomp! ~v\n" v))
       #:sprite
       (λ ()
         (sprite x y
                 (sprite-idx csd 'fish)
                 #:m m
                 #:layer 0
                 #:theta
                 (if (unbox fish-spin?-box)
                   (fl* (fl/
                         (fx->fl
                          (fxmodulo (fxquotient
                                     (- (current-milliseconds)
                                        (unbox fish-spin?-box))
                                     10)
                                    360))
                         360.0)
                        (fl* 2.0 pi))
                   0.0)))))
    (define (make-a-weird-fish! x y)
      (make-a-std-fish!
       x y 3.0
       (λ (_)
         (spriteboard-orient! sb
                              (match (spriteboard-orient sb)
                                ['portrait 'landscape]
                                ['landscape 'portrait])))))
    (define (make-an-odd-fish! x y)
      (make-a-std-fish!
       x y 3.0
       (λ (_)
         (with-spriteboard-ignore sb
           (play-sound! #"sample.m4a")
           (sleep 3)
           (play-sound! #"sample.m4a"))
         (void))))
    (define (make-a-fish! x y)
      (make-a-std-fish!
       x y 4.0
       (λ (fish-spin?-box)
         (play-sound! #"sample.m4a")
         (play-sound! #"sample.m4a")
         (cond
           [(unbox fish-spin?-box)
            (set-box! fish-spin?-box #f)]
           [else
            (set-box! fish-spin?-box (current-milliseconds))]))))
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
                 #:layer 1
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
       (λ (v)
         (printf "About to play\n")
         (sync (play-sound! #"sample.m4a"))
         (printf "Chomp chump ~v ~v\n" i v))
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
          [(and (= x 2) (= y 3))
           make-a-weird-fish!]
          [(and (= x 4) (= y 3))
           make-an-odd-fish!]
          [else
           make-a-fish!]))
      (maker! (fl* (fl/ (fl+ 0.5 (fx->fl x)) (fx->fl N)) (fx->fl W))
              (fl* (fl/ (fl+ 0.5 (fx->fl y)) (fx->fl N)) (fx->fl H))))

    (spriteboard-clickable!
     sb
     #:sprite (meta-sprite (list 0.0 0.0 0.0 0.0)
                           (text-render "Text!"
                                        (fl/ (fx->fl W) 2.0) (fl/ (fx->fl H) 4.0)
                                        #:layer 2
                                        #:mx 10.0 #:my 10.0
                                        #:r 255 #:g 0 #:b 0)))

    (spriteboard-draggable!
     sb
     #:init-x (fl/ (fx->fl W) 4.0)
     #:init-y (fl/ (fx->fl H) 2.0)
     #:sprite
     (λ (dragging? x y)
       (meta-sprite*
        csd
        (text-render "Drag Me"
                     x y
                     #:layer 3
                     #:mx 10.0 #:my 10.0
                     #:r (if dragging? 0 255)
                     #:g (if dragging? 255 0)
                     #:b 0)))))
  
  (make-spriteboard W H csd render initialize!))
