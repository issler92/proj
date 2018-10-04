#lang racket/base

(require
  racket/string
  racket/list)

(provide
 (struct-out question)
 (struct-out item)
 (struct-out article)
 (struct-out document)
 (struct-out node)
 (struct-out model-result)
 (struct-out model-result-simple)
 document-statement
 document-type
 node-vector)


; (question integer? boolean? string? string? item?)
(struct question (number answer area statement items) #:transparent)

; (item symbol? string?)
(struct item (letter statement [question-number #:auto #:mutable]) #:auto-value 0 #:transparent)

; (article string? integer? string?)
(struct article (law number statement) #:transparent)
;obs: norma > titulo > capitulo > artigo > inciso, paragrafo > alinea, item

; (model-result document? number? document? document? symbol? bool?)
(struct model-result (question min-dist best-art best-ans correct-answer correct?) #:transparent)

; (model-result-simple number? number? string? number? symbol? symbol? bool?)
(struct model-result-simple (question min-dist best-law best-art best-ans correct-answer correct?) #:transparent)

;(node document?)
(struct node (document [neineighbors #:mutable #:auto])
  #:auto-value (list)
  #:guard (lambda (doc name)
            (unless (document? doc)
              (error "not valid document"))
            doc)
  #:transparent)

(define (node-vector node)
  (document-rep (node-document node)))

; (document (or question? item? article?) )
(struct document (source [rep #:auto #:mutable])
  #:auto-value  null
  #:guard (lambda (s name)
            (unless (or (question? s) (item? s) (article? s))
              (error "not valid source"))
            s)
  #:transparent)

(define (document-statement doc)
  (let ([source (document-source doc)])
    (cond [(item? source) (item-statement source)]
          [(question? source) (question-statement source)]
          [(article? source) (article-statement source)])))

(define (document-type doc)
  (let ([source (document-source doc)])
    (cond [(item? source) 'item]
          [(question? source) 'question]
          [(article? source) 'article]
          [else (error "undentified data structure")])))

(define i1 (item 'a "string item 1"))
(define i2 (item 'b "string item 2"))
(define i3 (item 'a "string item 1"))
(define q1 (question 1 'a "ethics" "string question 1" (list i1 i2)))
(define a1 (article "lei8096" 1 "string article 1"))
(define doc-item (document i1))
(define doc-qt (document q1))
(define doc-art (document a1))
