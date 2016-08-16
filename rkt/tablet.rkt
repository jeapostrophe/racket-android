#lang racket/base
(require racket/fixnum
         racket/flonum
         racket/match
         racket/async-channel
         racket/stxparam
         opengl
         (only-in ffi/unsafe ffi-lib)
         mode-lambda/backend/gl
         lux
         lux/chaos
         "shared.rkt")
(require (for-syntax racket/base
                     syntax/parse))

(set-gl-procedure-loader!
 (load-get-proc-address (ffi-lib #f) '()))
(gl-backend-version 'es3.2)

(struct touch-chaos (event-ch video-b label!)
  #:methods gen:chaos
  [(define (chaos-yield c e)
     (sync e))
   (define (chaos-event c)
     (touch-chaos-event-ch c))
   (define (chaos-output! c o)
     (set-box! (touch-chaos-video-b c) o))
   (define (chaos-label! c l)
     ((touch-chaos-label! c) l))
   (define (chaos-stop! c)
     (void))])

(define (platform-play-sound! pl p)
  ((vector-ref pl 0) p))
(define (platform-label! pl l)
  ((vector-ref pl 1) l))
(define (platform-draw-frame-done! pl)
  ((vector-ref pl 2)))

(define (make-touch #:platform platform)
  (define video-b (box void))
  (define event-ch (make-async-channel))
  (define the-fake-dc #f)
  (define (make-fake-dc)
    (let ()
      (local-require racket/class)
      (define fake-glctx
        (new
         (class* object% ()
           (super-new)

           (define/public (call-as-current f)
             (f))
           (define/public (swap-buffers)
             (void)))))
      (new
       (class* object% ()
         (super-new)

         (define/public (get-gl-context)
           fake-glctx)))))
  (define current-w 2560)
  (define current-h 1512)
  (define (label! l)
    (platform-label! platform l))

  (define (receive-rpc! r)
    (match r
      ['onDrawFrame
       ((unbox video-b) current-w current-h the-fake-dc)
       (platform-draw-frame-done! platform)]
      [(vector 'onSurfaceChanged w h)
       (set! current-w w)
       (set! current-h h)]
      ['onSurfaceCreated
       (set! the-fake-dc (make-fake-dc))]
      [(vector 'onTouchEvent 0 x y)
       (async-channel-put event-ch (vector 'down x y))]
      [(vector 'onTouchEvent 1 x y)
       (async-channel-put event-ch (vector 'up x y))]
      [(vector 'onTouchEvent 2 x y)
       (async-channel-put event-ch (vector 'drag x y))]
      [x
       (void)]))
  (values (touch-chaos event-ch video-b label!)
          receive-rpc!))

(define ((make-make-receive-rpc!
          #:make-app make-app)
         #:platform platform)
  (define-values (touch-chaos touch-rpc!)
    (make-touch #:platform platform))
  (define app-t
    (thread
     (λ ()
       (call-with-chaos
        touch-chaos
        (λ ()
          (fiat-lux
           (make-app
            #:play-sound!
            (λ (p) (platform-play-sound! platform p)))))))))
  touch-rpc!)

(define (ib->i b s)
  (integer-bytes->integer b #f (system-big-endian?) s (fx+ s 4)))
(define (fb->r b s)
  (floating-point-bytes->real b (system-big-endian?) s (fx+ s 4)))

(define (parse-rpc call b)
  (match call
    [0
     'onDrawFrame]
    [1
     (vector 'onSurfaceChanged (ib->i b 4) (ib->i b 8))]
    [2
     'onSurfaceCreated]
    [3
     (vector 'onTouchEvent (ib->i b 4) (fb->r b 8) (fb->r b 12))]))

(define ((make-run-app make-rpc!) in rpc-size platform)
  (printf "RPC Stream initializing\n")
  (define rpc! (make-rpc! #:platform platform))
  (let loop ()
    (define b (read-bytes rpc-size in))
    (cond
      [(eof-object? b)
       (eprintf "RPC Stream Ended")]
      [else
       (rpc! (parse-rpc (ib->i b 0) b))
       (loop)])))

(define-syntax (define-app stx)
  (syntax-parse stx
    [(_ . make-app-args)
     (syntax/loc stx
       (begin
         (define run-app
           (make-run-app
            (make-make-receive-rpc!
             #:make-app
             (do-make-app . make-app-args))))
         (provide run-app)))]))

(provide define-app
         csd
         play-sound!
         render)
