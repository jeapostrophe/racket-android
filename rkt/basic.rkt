#lang racket/base
(require racket/fixnum
         racket/flonum
         racket/math
         racket/match
         racket/async-channel
         opengl
         (only-in ffi/unsafe ffi-lib)
         mode-lambda
         mode-lambda/backend/gl
         lux
         lux/chaos)

;; XXX Cleanup

(define (flsqr x)
  (fl* x x))
(define (fldist x1 x2 y1 y2)
  (flsqrt
   (fl+ (flsqr (fl- x1 x2))
        (flsqr (fl- y1 y2)))))

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
  (define (label! l)
    (platform-label! platform l))

  (define last-click-t 0.0)
  (define last-click-x 0.0)
  (define last-click-y 0.0)
  ;; XXX Magic numbers with no reason behind them
  (define CLICK-THRESHOLD-T 125.0)
  (define CLICK-THRESHOLD-D 30.0)
  (define (receive-rpc! r)
    (match r
      ['onDrawFrame
       ((unbox video-b) current-w current-h fake-dc)
       (platform-draw-frame-done! platform)]
      [(vector 'onSurfaceChanged w h)
       (set! current-w w)
       (set! current-h h)]
      [(vector 'onTouchEvent 0 x y)
       (set! last-click-t (current-inexact-milliseconds))
       (set! last-click-x x)
       (set! last-click-y y)]
      [(vector 'onTouchEvent 1 x y)
       (define this-t
         (fl- (current-inexact-milliseconds) last-click-t))
       (define this-d
         (fldist last-click-x x last-click-y y))
       (cond
         [(and (fl< this-t CLICK-THRESHOLD-T)
               (fl< this-d CLICK-THRESHOLD-D))
          (async-channel-put
           event-ch
           (vector 'click
                   (fl/ (fl+ last-click-x x) 2.0)
                   (fl/ (fl+ last-click-y y) 2.0)))]
         [else
          (printf "potential click ignored: t(~v) d(~v)" this-t this-d)])]
      [x
       (void)]))
  (values (touch-chaos event-ch video-b label!)
          receive-rpc!))

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
;; XXX Actually implement something useful
(define (make-app
         #:play-sound! play-sound!)
  (define W 2560)
  (define H 1688)
  
  (define fish-idx (sprite-idx csd 'fish))
  (define fish-m 3.0)
  (define fish-x (fl/ (fx->fl W) 2.0))
  (define fish-y (fl/ (fx->fl H) 2.0))
  (define render
    (stage-draw/dc csd W H))
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
                               50)
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

(define (make-receive-rpc!
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
