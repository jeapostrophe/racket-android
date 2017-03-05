#lang racket/base
(require racket/gui
         racket/runtime-path
         racket/async-channel
         racket/class
         racket/flonum
         racket/fixnum
         racket/generic
         racket/async-channel
         racket/file
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
  (struct msg (p s))
  (define ch (make-async-channel))
  (define t
    (thread
     (λ ()
       (let loop ()
         (match-define (msg p s) (async-channel-get ch))
         (play-sound (build-path asset-path (bytes->path p)) #f)
         (semaphore-post s)
         (loop)))))
  (λ (p)
    (define s (make-semaphore))
    (async-channel-put ch (msg p s))
    s))

(gl-backend-version '3.3)

(define-runtime-path drive-path "../etc/drive")
(define (sim-drive-read p)
  (with-handlers ([exn:fail? (λ (x) #f)])
    (file->bytes (build-path drive-path p))))
(define (sim-drive-write! p c)
  (with-handlers ([exn:fail? (λ (x) #f)])
    (display-to-file c (build-path drive-path p) #:exists 'replace)
    #t))

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
  (define event-t
    (thread
     (λ ()
       (define from-ch (chaos-event the-gui))
       (let loop ()
         (match (sync from-ch)
           [(list 'resize act-W act-H)
            (async-channel-put event-ch (vector 'resize act-W act-H))]
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
                (vector type (fx->fl (send me get-x)) (fx->fl (send me get-y))))
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
                #:play-sound! (make-racket/gui-play-sound!)
                #:drive-read sim-drive-read
                #:drive-write! sim-drive-write!)))))))]))

(provide define-app
         define-static-font
         csd
         play-sound!
         drive-read
         drive-write!
         render)
