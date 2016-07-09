#lang racket/base

(define (go)
  (format "This is Racket running on Android at ~v, can you believe it!?!"
          (current-seconds)))

(provide go)
