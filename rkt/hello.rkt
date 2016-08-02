#lang racket/base

(define (go)
  (printf "No output\n")
  (format "We can do a lot of work in Racket ~a"
          (current-seconds)))

(provide go)

;; <- initialize
;; <- touch!
;; -> audio!
;; <- frame

;; big-bang -> 
