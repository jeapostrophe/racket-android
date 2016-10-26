#lang racket/base
(require racket/gui
         racket/runtime-path
         racket/async-channel
         racket/class
         racket/flonum
         racket/fixnum
         racket/generic
         racket/async-channel
         lux
         lux/chaos
         lux/chaos/gui
         mode-lambda/backend/gl
         mode-lambda/backend/lib
         "shared.rkt"
         (for-syntax racket/base
                     syntax/parse))

(define-runtime-path asset-path "../project/app/src/main/assets/")
(define (make-racket/gui-play-sound!)
  (define ch (make-async-channel))
  (define t
    (thread
     (λ ()
       (let loop ()
         (define p (async-channel-get ch))
         (play-sound (build-path asset-path (bytes->path p)) #f)
         (loop)))))
  (λ (p)
    (async-channel-put ch p)))

(gl-backend-version '3.3)

(struct simulator (event-ch inner-chaos)
  #:methods gen:chaos
  [(define/generic super-start! chaos-start!)
   (define (chaos-start! c) (super-start! (simulator-inner-chaos c)))

   (define/generic super-yield chaos-yield)
   (define (chaos-yield c e) (super-yield (simulator-inner-chaos c) e))

   (define (chaos-event c) (simulator-event-ch c))

   (define/generic super-output! chaos-output!)
   (define (chaos-output! c o) (super-output! (simulator-inner-chaos c) o))

   (define/generic super-label! chaos-label!)
   (define (chaos-label! c lab) (super-label! (simulator-inner-chaos c) lab))

   (define/generic super-swap! chaos-swap!)
   (define (chaos-swap! c t) (super-swap! (simulator-inner-chaos c) t))

   (define/generic super-stop! chaos-stop!)
   (define (chaos-stop! c) (super-stop! (simulator-inner-chaos c)))])
(define (make-simulator sim-W sim-H the-gui)
  (define mouse-event%? (is-a?/c mouse-event%))
  (define event-ch (make-async-channel))
  (define scale 1.0)
  (define inset-left 0.0)
  (define inset-bot 0.0)
  (define event-t
    (thread
     (λ ()
       (define from-ch (chaos-event the-gui))
       (let loop ()
         (match (sync from-ch)
           [(list 'resize act-W act-H)
            (set! scale (compute-nice-scale 1.0 act-W sim-W act-H sim-H))
            (set! inset-left (fl/ (fl- (fx->fl act-W) (fl* scale (fx->fl sim-W))) 2.0))
            (set! inset-bot (fl/ (fl- (fx->fl act-H) (fl* scale (fx->fl sim-H))) 2.0))]
           [(? mouse-event%? me)
            (define type
              (cond
                [(send me dragging?)
                 'drag]
                [(send me button-down?)
                 'down]
                [(send me button-up?)
                 'up]
                [else
                 #f]))
            (when type
              (define ne
                (vector type
                        (fl/ (fl- (fx->fl (send me get-x))
                                  inset-left)
                             scale)
                        (fl/ (fl- (fx->fl (send me get-y))
                                  inset-bot)
                             scale)))
              (async-channel-put event-ch ne))]
           [_
            (void)])
         (loop)))))
  (simulator event-ch the-gui))

(define-syntax (define-app stx)
  (syntax-parse stx
    [(_ . (~and (([W:id W-v:expr] [H:id H-v:expr]) . body)
                make-app-args))
     (syntax/loc stx
       (begin
         (define make-app
           (do-make-app . make-app-args))
         (module+ main
           (call-with-chaos
            (make-simulator W-v H-v (make-gui #:mode gui-mode))
            (λ ()
              (fiat-lux
               (make-app
                #:play-sound! (make-racket/gui-play-sound!))))))))]))

(provide define-app
         define-static-font
         csd
         play-sound!
         render)
