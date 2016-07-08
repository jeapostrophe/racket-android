#lang racket/base

(define (go)
  (format "This is Racket on ~v" (system-type 'os)))

(provide go)
