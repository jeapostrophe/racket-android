#lang racket/base
(require racket/stxparam
         mode-lambda
         mode-lambda/backend/gl
         (for-syntax racket/base
                     racket/runtime-path
                     racket/file
                     syntax/parse))

(define-syntax-parameter play-sound!
  (λ (stx) (raise-syntax-error 'play-sound! "Illegal outside define-app" stx)))
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
       (λ (#:play-sound! local-play-sound!)
         (define W W-v)
         (define H H-v)
         (define local-render (stage-draw/dc csd W H))
         (syntax-parameterize
             ([play-sound! (make-rename-transformer #'local-play-sound!)]
              [render (make-rename-transformer #'local-render)])
           . body)))]))

(provide (all-defined-out))
