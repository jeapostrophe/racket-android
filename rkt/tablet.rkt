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
(gl-backend-version 'es3.1)

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
(define (platform-drive-read pl p)
  ((vector-ref pl 3) p))
(define (platform-drive-write! pl p c)
  ((vector-ref pl 4) p c))

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
       (eprintf "onSurfaceChanged ~a ~a\n" w h)
       (set! current-w w)
       (set! current-h h)
       (async-channel-put event-ch (vector 'resize w h))]
      ['onSurfaceCreated
       (set! the-fake-dc (make-fake-dc))]
      [(vector 'onTouchEvent 0 x y)
       (async-channel-put event-ch (vector 'down x y))]
      [(vector 'onTouchEvent 1 x y)
       (async-channel-put event-ch (vector 'up x y))]
      [(vector 'onTouchEvent 2 x y)
       (async-channel-put event-ch (vector 'drag x y))]
      [(vector 'soundComplete id)
       (define s (hash-ref sound->sema id #f))
       (when s
         (semaphore-post s)
         (hash-remove! sound->sema id))]
      [(vector 'setDriveStatus m)
       (eprintf "setDriveStatus ~a\n" m)
       (set-drive-status?! (zero? m))
       (void)]
      [x
       (void)]))

  (define sound->sema (make-hasheq))
  (define (play-sound! p)
    (define s (make-semaphore))
    (define id (platform-play-sound! platform p))
    (hash-set! sound->sema id s)
    s)

  (define drive-status?-ch (make-channel))
  (define drive-status!-ch (make-channel))
  (define (get-drive-status?)
    (define reply-ch (make-channel))
    (channel-put drive-status?-ch reply-ch)
    (channel-get reply-ch))
  (define (set-drive-status?! v)
    (channel-put drive-status!-ch v))
  (define drive-status?-t
    (thread
     (λ ()
       (let loop ([status? (channel-get drive-status!-ch)])
         (sync
          (handle-evt drive-status!-ch loop)
          (handle-evt drive-status?-ch
                      (λ (reply-ch)
                        (channel-put reply-ch status?)
                        (loop status?))))))))
  
  (define (drive-read p)
    (and (get-drive-status?)
         (platform-drive-read platform p)))
  (define (drive-write! p c)
    (and (get-drive-status?)
         (platform-drive-write! platform p c)))
  
  (values (touch-chaos event-ch video-b label!)
          receive-rpc!
          play-sound!
          drive-read
          drive-write!))

(define ((make-make-receive-rpc!
          #:make-app make-app)
         #:platform platform)
  (define-values (touch-chaos
                  touch-rpc!
                  touch-play-sound!
                  touch-drive-read
                  touch-drive-write!)
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
            touch-play-sound!
            #:drive-read
            touch-drive-read
            #:drive-write!
            touch-drive-write!)))))))
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
     (vector 'onTouchEvent (ib->i b 4) (fb->r b 8) (fb->r b 12))]
    [4
     (vector 'soundComplete (ib->i b 4))]
    [5
     (vector 'setDriveStatus (ib->i b 4))]))

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
         (provide run-app)
         (module+ main)))]))

(provide define-app
         define-static-font
         csd
         play-sound!
         drive-read
         drive-write!
         render)
