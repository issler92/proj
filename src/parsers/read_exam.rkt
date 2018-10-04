#lang racket/base

(require
  racket/string
  racket/list
  txexpr
  xml
  xml/path)

(require (except-in "../data-structures.rkt" struct:document document document? document-statement document-type))

(provide read-exam)

; xml:documen? -> xexpr?
(define (exam-xml->xexpr fp)
  (with-input-from-file fp
    (lambda () (xml->xexpr (document-element (read-xml (current-input-port)))))
    #:mode 'text))

; xexpr -> (listof question?)
(define (xexpr->questions xe)
  (define (get-letter item)
    (string->symbol (string-upcase (attr-ref item 'letter))))
  (define (get-answer xelems)
    (for/or ([elem (in-list xelems)])
      (and (txexpr? elem)
           (equal? (attr-ref elem 'correct "false") ; only items have correct attr
                   "true")
           (get-letter elem))))
  (define (get-statement xelems)
    (for/or ([elem (in-list xelems)])
      (and (txexpr? elem)
           (eq? (get-tag elem) 'statement)
           (string-join (get-elements elem)))))
  ; (listof xelem?) -> (listof item?)
  (define (get-items xelems)
    (for/list ([elem (in-list xelems)]
               #:when (and (txexpr? elem) (eq? (get-tag elem) 'item)))
      (item (get-letter elem)
            (string-join (get-elements elem)))))
  (for/list ([xeq (in-list (get-elements xe))]
             #:unless (string? xeq))
    (let ([xelems (get-elements xeq)]
          [num (string->number (attr-ref xeq 'number))])
      (define items (get-items xelems))
      (map (lambda (item) (set-item-question-number! item num)) items)
      (question num
                (if (attr-ref xeq 'valid)
                    (get-answer xelems)
                    #f)
                (attr-ref xeq 'area)
                (get-statement xelems)
                items))))

;string? -> (listof question?))
(define (read-exam path)
  (xexpr->questions (exam-xml->xexpr path)))
