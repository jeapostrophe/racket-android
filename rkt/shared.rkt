#lang racket/base
(require racket/stxparam
         racket/contract/base
         mode-lambda
         mode-lambda/backend/gl
         (for-syntax racket/base
                     racket/runtime-path
                     racket/file
                     syntax/parse))

(define-syntax-parameter play-sound!
  (λ (stx) (raise-syntax-error 'play-sound! "Illegal outside define-app" stx)))
(define-syntax-parameter drive-read
  (λ (stx) (raise-syntax-error 'drive-read "Illegal outside define-app" stx)))
(define-syntax-parameter drive-write!
  (λ (stx) (raise-syntax-error 'drive-write! "Illegal outside define-app" stx)))
(define-syntax-parameter render
  (λ (stx) (raise-syntax-error 'render "Illegal outside define-app" stx)))

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

(define-syntax (define-static-font stx)
  (syntax-case stx ()
    [(_ i p)
     (quasisyntax/loc stx
       (define i
         #,(file->value (build-path here (syntax->datum #'p)))))]))

(define-syntax (do-make-app stx)
  (syntax-parse stx
    [(_ ([W:id W-v:expr] [H:id H-v:expr]) . body)
     (syntax/loc stx
       (λ (#:play-sound! local-play-sound!
           #:drive-read local-drive-read
           #:drive-write! local-drive-write!)
         (define pos 'platform)
         (define neg 'app)
         (define checked-play-sound!
           (contract (-> bytes? semaphore?) local-play-sound!
                     pos neg))
         (define checked-drive-read
           (contract (-> bytes? (or/c bytes? false/c)) local-drive-read
                     pos neg))
         (define checked-drive-write!
           (contract (-> bytes? bytes? boolean?) local-drive-write!
                     pos neg))
         (define W W-v)
         (define H H-v)
         (define local-render (stage-draw/dc csd W H 4))
         (syntax-parameterize
             ([play-sound! (make-rename-transformer #'checked-play-sound!)]
              [drive-read (make-rename-transformer #'checked-drive-read)]
              [drive-write! (make-rename-transformer #'checked-drive-write!)]
              [render (make-rename-transformer #'local-render)])
           . body)))]))

(provide (all-defined-out))
