#lang racket/base
(require racket/fixnum
         racket/match
         racket/async-channel
         opengl
         (only-in ffi/unsafe ffi-lib)
         mode-lambda
         mode-lambda/backend/gl
         lux
         lux/chaos)

(set-gl-procedure-loader!
 (let ()
   (define gpa (load-get-proc-address (ffi-lib #f) '()))
   (λ (name)
     (printf "Trying to load GL proc: ~v\n" name)
     (define ans (gpa name))
     (printf "Got ~v\n" ans)
     ans)))

(struct touch-chaos (event-ch video-b audio! label!)
  #:methods gen:chaos
  [(define (chaos-yield c e)
     (sync e))
   (define (chaos-event c)
     (touch-chaos-event-ch c))
   (define (chaos-output! c o)
     (match-define (cons video audio) o)
     (when audio ((touch-chaos-audio! c) audio))
     (set-box! (touch-chaos-video-b c) video))
   (define (chaos-label! c l)
     ((touch-chaos-label! c) l))
   (define (chaos-stop! c)
     (void))])

(define (platform-play-sound! pl p)
  ((vector-ref pl 0) p))
(define (platform-label! pl l)
  ((vector-ref pl 1) l))
(define (platform-set-gl-context! pl)
  ((vector-ref pl 2)))

(define (make-touch #:platform platform)
  (define video-b (box void))
  (define event-ch (make-async-channel))
  (define fake-dc
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
  (define (audio! p)
    (platform-play-sound! platform p))
  (define (label! l)
    (platform-label! platform l))
  (define (receive-rpc! r)
    (match r
      ['onDrawFrame
       ((unbox video-b) current-w current-h fake-dc)]
      [(vector 'onSurfaceChanged w h)
       (set! current-w w)
       (set! current-h h)]
      ['onSurfaceCreated
       (platform-set-gl-context! platform)]
      [x
       ;; XXX Parse out clicks
       (async-channel-put event-ch x)]))
  (values (touch-chaos event-ch video-b audio! label!)
          receive-rpc!))

;; XXX Actually implement something useful
(define W 1280)
(define H 756)
(require (for-syntax racket/base
                     racket/file
                     racket/runtime-path))
(begin-for-syntax
  (define-runtime-path here "."))
(define-syntax (define-static-csd stx)
  (syntax-case stx ()
    [(_ i p)
     (quasisyntax/loc stx
       (define i
         (load-csd/bs
          #,(file->bytes (build-path here (syntax->datum #'p))))))]))
(define-static-csd csd "csd.rktd.gz")
(struct app
    (render)
    #:methods gen:word
    [(define (word-fps w) 30.0)
     (define (word-label w ft)
       (lux-standard-label "App" ft))
     (define (word-output w)
       (cons ((app-render w)
              (vector (layer (fx->fl (/ W 2)) (fx->fl (/ H 2)))
                      #f #f #f #f #f #f #f)
              '()
              (sprite 60.0 60.0 (sprite-idx csd 'fish))) #f))
     (define (word-event w e)
       (printf "~v\n" e)
       w)
     (define (word-tick w)
       w)])
(define (make-app)
  (app (stage-draw/dc csd W H)))

(define (make-receive-rpc!
         #:platform platform)
  ;; XXX Remove this
  (platform-play-sound! platform #"sample.m4a")
  (define-values (touch-chaos touch-rpc!)
    (make-touch #:platform platform))
  (define app-t
    (thread
     (λ ()
       (call-with-chaos
        touch-chaos
        (λ ()
          (fiat-lux (make-app)))))))
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

(define run-app (make-run-app make-receive-rpc!))
(provide run-app)
