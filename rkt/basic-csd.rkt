#lang racket/base
(require mode-lambda/static
         pict)

(define sd (make-sprite-db))
(add-sprite!/value
 sd 'fish
 (standard-fish 50 50))
(add-sprite!/value
 sd 'cloud
 (cloud 50 50))
(add-sprite!/value
 sd 'jack
 (jack-o-lantern 50))
(define static-csd
  (compile-sprite-db sd))

(module+ main
  (require racket/runtime-path)
  (define-runtime-path here ".")
  (save-csd! static-csd here))
