#lang racket/base

; calling (P) and (L) results in the 2010-01 test and the 8906 law, respectively.
(module+ main
  (require
    racket/cmdline
    racket/string
    racket/list
    txexpr
    math/array
    "parsers/read_exam.rkt"
    "parsers/read_articles.rkt"
    "tfidf/tfidf.rkt"
    "graph/graph.rkt"
    "graph/dist.rkt"
    "data-structures.rkt"
    )

  (define articles-path (make-parameter "data/raw/articles/"))
  (define exams-path (make-parameter "data/raw/exams/"))
  (define output-type (make-parameter "simple"))
  (define dist-fun (make-parameter "dist"))

  (define exam-path
    (command-line
     #:program "projeto-eda"
     #:usage-help
     "Solve OAB exams through tf-idf"
     "---------------------"
     #:once-each
     [("-a" "--articles-path") lawspath
                               "Setting path to dir where the laws are archived"
                               (articles-path lawspath)]
     [("-e" "--exams-path") exampath
                            "Setting path to dir where the laws are archived"
                            (exams-path exampath)]
     [("-o" "--output-type") outype
                             "Set the type of output: simple, struct-simple or complete"
                             (output-type outype)]
     [("-d" "--distance-function") distance-function
                                   "Set the distance function: dist (euclidian) or cos-dist (cosine similarities)"
                                   (dist-fun distance-function)]
     #:args (exam)

     (string-append (exams-path) exam)))

  (define dist-f
    (let ([d (string->symbol (dist-fun))])
      (cond [(eq? d 'dist) dist]
            [(eq? d 'cos-dist) cos-dist]
            [else (error "Not a valid distance function")])))

  ;(listof question?) -> (listof (listof documents?))
  (define (prepare-one-exam exam)
    (for/fold ([questions-answers null]
               #:result (reverse questions-answers))
              ([question exam])
      (cons (cons (document question)
                  (map document (question-items question)))
            questions-answers)))

  ;(listof (listof article?) -> (listof documents?)
  (define (prepare-articles art)
    (map document (flatten art)))

  ;(listof documents) and (listof documents) -> (listof documents), (listof documents) and (listof documents)
  (define (apply-tfidf question-item-docs laws-docs)
    (define updated-docs (second (tf-idf (append question-item-docs laws-docs))))
    (for/fold ([question null]
               [items null]
               [laws null]
               #:result (values (reverse question) (reverse items) (reverse laws)))
              ([doc (in-list updated-docs)])
      (cond [(eq? (document-type doc) 'question) (values (cons doc question) items laws)]
            [(eq? (document-type doc) 'item) (values question (cons doc items) laws)]
            [(eq? (document-type doc) 'article) (values question items (cons doc laws))])))

  (define (apply-model list-question-item-docs laws-docs)
    (for/fold ([output null]
               #:result (reverse output))
              ([question-item-docs list-question-item-docs])
      (define-values (q i a) (apply-tfidf question-item-docs laws-docs))
      (define-values (min-dist best-art-node best-ans-node)
        (get-distance-article-answer (first (map node q))
                                     (map node a)
                                     (map node i)
                                     dist-f))
      (define-values (question-doc best-art-doc best-ans-doc)
        (values (first question-item-docs) (node-document best-art-node) (node-document best-ans-node)))
      (define-values (correct-answer model-predicted-answer)
        (values (question-answer (document-source question-doc)) (item-letter (document-source best-ans-doc))))
      (cons (model-result question-doc min-dist best-art-doc best-ans-doc correct-answer (eq? correct-answer model-predicted-answer))
            output)))


  (define (simple-output output)
    (list
     (string-join (list "Question: " (number->string (question-number (document-source (model-result-question output))))))
     (model-result-min-dist output)
     (string-join (list "Lei:" (article-law (document-source (model-result-best-art output))) "| Artigo:" (number->string (article-number (document-source (model-result-best-art output))))))
     (item-letter (document-source (model-result-best-ans output)))
     (model-result-correct-answer output)
     (model-result-correct? output)))

  (define (struct-simple-output output)
    (model-result-simple
     (question-number (document-source (model-result-question output)))
     (model-result-min-dist output)
     (article-law (document-source (model-result-best-art output)))
     (article-number (document-source (model-result-best-art output)))
     (item-letter (document-source (model-result-best-ans output)))
     (model-result-correct-answer output)
     (model-result-correct? output)))

  (define (convert-output output-list output-type)
    (let ([ot (string->symbol output-type)])
      (cond [(eq? ot 'simple) (for ((output output-list))
                                (displayln (simple-output output)))]
            [(eq? ot 'struct-simple) (for ((output output-list))
                                       (displayln (struct-simple-output output)))]
            [(eq? ot 'complete) (for ((output output-list))
                                  (displayln output))]
            [else (error "Not a valid output format")])))

  (define (main articles-path exam-path output-type)
    (let ([list-questions (prepare-one-exam (read-exam exam-path))]
          [list-articles (prepare-articles (read-articles articles-path))])
      (convert-output (apply-model list-questions list-articles)
                      output-type)))

  (main (articles-path) exam-path (output-type)))
