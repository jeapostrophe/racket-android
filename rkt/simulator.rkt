#lang racket/base
(require racket/gui
         racket/runtime-path
         racket/async-channel
         racket/class
         racket/flonum
         racket/fixnum
         racket/generic
         lux
         lux/chaos
         lux/chaos/gui
         mode-lambda/backend/gl
         mode-lambda/backend/lib
         "shared.rkt"
         (for-syntax racket/base
                     syntax/parse))

(define-runtime-path asset-path "../project/app/src/main/assets/")
(define (racket/gui-play-sound! p)
  (play-sound (build-path asset-path (bytes->path p)) #t))

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
(define (make-simulator W H the-gui)
  (define mouse-event%? (is-a?/c mouse-event%))
  (define event-ch (make-async-channel))
  (define scale 1.0)
  (define event-t
    (thread
     (λ ()
       (define from-ch (chaos-event the-gui))
       (let loop ()
         (match (sync from-ch)
           [(list 'resize w h)
            (set! scale (compute-nice-scale 1.0 w W h H))]
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
                        (fl/ (fx->fl (send me get-x)) scale)
                        (fl/ (fx->fl (send me get-y)) scale)))
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
                #:play-sound! racket/gui-play-sound!)))))))]))

(provide define-app
         csd
         play-sound!
         render)
