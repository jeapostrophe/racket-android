#lang racket/base
(require racket/flonum
         racket/fixnum
         racket/match
         racket/contract/base
         mode-lambda
         lux)

(define (flsqr x)
  (fl* x x))
(define (fldist x1 x2 y1 y2)
  (flsqrt
   (fl+ (flsqr (fl- x1 x2))
        (flsqr (fl- y1 y2)))))

(struct *obj (m-spr click! alive?))
(define (object-spr o)
  (define m (*obj-m-spr o))
  (if (procedure? m)
      (m)
      m))
(define (object-click! o)
  ((*obj-click! o)))
(define (object-alive? o)
  ((*obj-alive? o)))

(struct spriteboard (metatree meta->tree) #:mutable)
(define (make-the-spriteboard)
  (spriteboard null (make-hasheq)))
(define (spriteboard-tree sb)
  (match-define (spriteboard mt m->t) sb)
  (hash-clear! m->t)
  (for/list ([m (in-list mt)])
    (define t (object-spr m))
    (hash-set! m->t m t)
    t))
(define (spriteboard-gc! sb)
  (match-define (spriteboard mt m->t) sb)
  (set-spriteboard-metatree!
   sb
   (for/fold ([mt null])
             ([m (in-list mt)])
     (if (object-alive? m)
         (cons m mt)
         mt))))

(define (spriteboard-new! sb
                          #:sprite m-spr
                          #:click! [click! void]
                          #:alive? [alive? (λ () #t)])
  (set-spriteboard-metatree!
   sb
   (cons
    (*obj m-spr click! alive?)
    (spriteboard-metatree sb))))

(define (sprite-inside? csd t x y)
  (define t-idx (sprite-data-spr t))
  
  (define tcx (sprite-data-dx t))
  (define sw (fx->fl (sprite-width csd t-idx)))
  (define tw (fl* (sprite-data-mx t) sw))
  (define thw (fl/ tw 2.0))
  (define x-min (fl- tcx thw))
  (define x-max (fl+ tcx thw))

  (define tcy (sprite-data-dy t))
  (define sh (fx->fl (sprite-height csd t-idx)))
  (define th (fl* (sprite-data-my t) sh))
  (define thh (fl/ th 2.0))
  (define y-min (fl- tcy thh))
  (define y-max (fl+ tcy thh))
  
  (and (fl<= x-min x)
       (fl<= x x-max)
       (fl<= y-min y)
       (fl<= y y-max)))

(define (make-spriteboard W H csd render initialize!)
  (define layer-c
    (vector (layer (fx->fl (/ W 2)) (fx->fl (/ H 2)))
            (layer (fx->fl (/ W 2)) (fx->fl (/ H 2)))
            #f #f #f #f #f #f))
  (define the-sb (make-the-spriteboard))
  (struct app ()
    #:methods gen:word
    [(define (word-fps w) 30.0)
     (define (word-label w ft)
       (lux-standard-label "Spriteboard" ft))
     (define (word-output w)
       (render layer-c '() (spriteboard-tree the-sb)))
     (define (word-event w e)
       (match e
         [(vector 'down x y)
          (define m->t (spriteboard-meta->tree the-sb))
          (let/ec esc
            (for ([m (in-list (spriteboard-metatree the-sb))])
              (define t (hash-ref m->t m #f))
              (when (and t
                         (sprite-inside? csd t x y))
                (object-click! m)
                (esc))))]
         [(vector 'drag x y)
          (printf "XXX implement drag movement\n")]
         [(vector 'up x y)
          (printf "XXX implement dropping\n")])
       w)
     (define (word-tick w)
       (spriteboard-gc! the-sb)
       w)])
  (initialize! the-sb)
  (app))

(provide
 (contract-out
  [spriteboard-new!
   (->* (spriteboard?
         #:sprite (or/c sprite-data? (-> sprite-data?)))
        (#:click! (-> void?)
         #:alive? (-> boolean?))
        void?)]
  [make-spriteboard
   (-> real? real? compiled-sprite-db? procedure? (-> spriteboard? void?)
       any/c)]))
