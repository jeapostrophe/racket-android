#lang racket/base
(require "xprize/app.rkt")
(provide (all-from-out "xprize/app.rkt"))
(module+ main
 (require (submod "xprize/app.rkt" main)))
