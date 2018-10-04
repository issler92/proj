#lang racket/base

(require
  math/matrix
  racket/string
  racket/list
  racket/set
  math/array
  "../data-structures.rkt"
  "stopwords.rkt"
  "../parsers/read_articles.rkt"
  "../parsers/read_exam.rkt"
  "../parsers/read_law.rkt")
(require (only-in data-science remove-punctuation tdm dtm tf-matrix transpose-tf-matrix idf-matrix transpose-idf-matrix))


(provide tf-idf tf-idf-transpose tf-transpose)

(define (treat-strings string)
  (filter-not (λ (e) (set-member? stopwords e))
              ((compose string-split remove-punctuation string-downcase string-normalize-spaces) string)))


; string --> (list of (list of 'str' float))
(define (string->tokens string)
  (let* ((list-string (treat-strings string))
         (total (length list-string)))
    (map (lambda (x) (list (first x) (length x)))
         (group-by (lambda (x) x) list-string))))

; (list of documents) --> list of (list of strings) and (list of documents)
(define (tf-idf corpus)
  (define-values (matrix-cols corpus-array)
    (apply values (apply dtm
                         (map string->tokens
                              (map document-statement corpus)))))
  (list matrix-cols (for/fold ([updated-corpus null]
                               #:result (reverse updated-corpus))
                              ([doc (in-list corpus)]
                               [i (in-naturals)])
                      (define doc-updated (document (document-source doc)))
                      (set-document-rep! doc-updated
                                         (array->vector
                                          (array-slice-ref corpus-array
                                                           (list i (::)))))
                      (cons doc-updated updated-corpus))))

(define (tf-idf-transpose corpus)
  (define-values (matrix-cols corpus-array)
    (apply values (apply tdm
                         (map string->tokens
                              (map document-statement corpus)))))
  (list matrix-cols (for/fold ([updated-corpus null]
                               #:result (reverse updated-corpus))
                              ([doc (in-list corpus)]
                               [i (in-naturals)])
                      (define doc-updated (document (document-source doc)))
                      (set-document-rep! doc-updated
                                         (array->vector
                                          (array-slice-ref corpus-array
                                                           (list i (::)))))
                      (cons doc-updated updated-corpus))))
(define (tf-transpose corpus)
  (define-values (matrix-cols corpus-array)
    (apply values (apply transpose-tf-matrix
                         (map string->tokens
                              (map document-statement corpus)))))
  (list matrix-cols (for/fold ([updated-corpus null]
                               #:result (reverse updated-corpus))
                              ([doc (in-list corpus)]
                               [i (in-naturals)])
                      (define doc-updated (document (document-source doc)))
                      (set-document-rep! doc-updated
                                         (array->vector
                                          (array-slice-ref corpus-array
                                                           (list i (::)))))
                      (cons doc-updated updated-corpus))))




;Definições para exemplos e testes
(define i1 (item 'a "string item 1"))
(set-item-question-number! i1 1)
(define i2 (item 'b "string item 2"))
(set-item-question-number! i2 1)
(define q1 (question 1 'a "ethics" "string question 1" (list i1 i2)))
(define a1 (article "lei8096" 1 "string article 1"))
(define doc-qt (document q1))
(define doc-item1 (document i1))
(define doc-item2 (document i2))
(define doc-art (document a1))
(define corpus (list doc-qt doc-item1 doc-item2 doc-art))



