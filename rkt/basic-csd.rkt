#lang racket/base
(require mode-lambda/static
         mode-lambda/text/static
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
(define f
  (load-font! sd
              #:family 'roman
              #:size 20))
(define static-csd
  (compile-sprite-db sd))

(module+ main
  (require racket/runtime-path
           racket/file)
  (define-runtime-path here ".")
  (save-csd! static-csd here)
  (write-to-file f (build-path here "csd-font.rktd")
                 #:exists 'replace))
