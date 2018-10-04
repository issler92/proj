#lang racket/base

(require
  racket/string
  racket/list
  txexpr
  xml
  xml/path)

(require (except-in "../data-structures.rkt" struct:document document document? document-statement document-type))

(provide read-law)

; xml:document? -> xexpr?
(define (lei-xml->xexpr fp)
  (with-input-from-file fp
    (lambda () (xml->xexpr (document-element (read-xml (current-input-port)))))
    #:mode 'text))

; xexpr? -> (listof string?)
(define (xexpr->articles-statement tx)
  ; txexpr? -> string?
  (define (txexpr->string tx)
    (cond
      [(txexpr? tx)
       (string-join (map txexpr->string (get-elements tx)))]
      [(string? tx)
       tx]
      [else ""]))
  ; xexpr? -> boolean?
  (define (artigo? tx)
    (and (txexpr? tx) (eq? (get-tag tx) 'artigo)))
  ; xexpr? -> string?
  (define (gather-artigo art)
    (txexpr->string art))
  ;;
  (cond
    [(artigo? tx)
     (list (gather-artigo tx))]
    [(txexpr? tx)
     (append-map xexpr->articles-statement (get-elements tx))]
    [else null]))

;(listof string?), string? -> (listof article?)
(define (articles-statement->articles list-of-articles law)
  (let ((counter 0))
    (for/list ((a list-of-articles))
      (set! counter (add1 counter))
      (article law counter a))))

;string? -> (listof (listof article?))
(define (read-law path)
  (define law-xml-files (map path->string (directory-list path)))
  (for/fold ([laws-articles null]
             #:result (reverse laws-articles))
            ((law-file law-xml-files))
    (cons (articles-statement->articles
           (xexpr->articles-statement (lei-xml->xexpr
                                       (string-append path law-file)))
           (first (string-split law-file ".xml")))
          laws-articles)))
