#lang racket/base
(require racket/fixnum
         racket/match
         racket/async-channel
         mode-lambda
         lux
         lux/chaos)

(struct touch-chaos (event-ch video-b audio!)
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
     ;; XXX Add to platform
     (void))
   (define (chaos-stop! c)
     (void))])

(define (make-touch #:play-sound! play-sound!)
  (define video-b (box void))
  (define event-ch (make-async-channel))
  (define (receive-rpc! r)
    (match r
      ['onDrawFrame
       ;; XXX Do the draw
       (void)]
      ['onSurfaceChanged
       ;; XXX Remember this
       (void)]
      ['onSurfaceCreated
       ;; XXX Do initialization
       (void)]
      [x
       ;; XXX Parse out clicks
       (async-channel-put event-ch x)]))
  (values (touch-chaos event-ch video-b play-sound!)
          receive-rpc!))

(struct app
    ()
    #:methods gen:word
    [(define (word-fps w) 30.0)
     (define (word-label w ft)
       (lux-standard-label "App" ft))
     (define (word-output w)
       (cons (void) #f))
     (define (word-event w e)
       (printf "Event is ~v\n" e)
       w)
     (define (word-tick w)
       w)])

(define (make-receive-rpc!
         #:play-sound! play-sound!)
  (play-sound! #"sample.m4a")
  (define-values (touch-chaos touch-rpc!)
    (make-touch #:play-sound! play-sound!))
  (define app-t
    (thread
     (λ ()
       (call-with-chaos
        touch-chaos
        (λ ()
          (fiat-lux (app)))))))
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

(define ((make-run-app make-rpc!) in rpc-size play-sound!)
  (printf "RPC Stream initializing\n")
  (define rpc! (make-rpc! #:play-sound! play-sound!))  
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
