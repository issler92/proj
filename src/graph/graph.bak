#lang racket

(require
  racket/block
  racket/trace
  racket/undefined
  data/heap
  dyoo-while-loop)

(require "dist.rkt"
         "../data-structures.rkt")

(provide
 dijkstra
 to-graph
 get-distance-article-answer
 node
 set-node-neineighbors!
 node-neineighbors)

;; retorna o dijkstra a partir de qualquer distancia
(define (dij-from dist)
  (define (Dijkstra graph source)
    (define (operator-less a b)
      (< (cdr a) (cdr b)))
    (define node-queue (make-heap operator-less))
    (heap-add! node-queue (cons source 0))

    (define distances-from-source (make-hash))
    (for ([node graph])
      (dict-set! distances-from-source node +inf.f))

    (dict-set! distances-from-source source 0)
    (define previous (make-hash))

    (while (not (zero? (heap-count node-queue)))
           (match-define (cons u u-dist) (heap-min node-queue))
           (define u-vector (node-vector u))
           (define u-neigs (node-neineighbors u))
           (heap-remove-min! node-queue)
           (for ([v u-neigs])
             (define v-vector (node-vector v))
             (define alt (+ u-dist (dist u-vector v-vector)))
             (if (< alt (dict-ref distances-from-source v))
                 (block
                  (dict-set! distances-from-source v alt)
                  (dict-set! previous v u)
                  (heap-add! node-queue (cons v alt)))
                 void)))

    (values distances-from-source previous))

  Dijkstra)

;; dijkstra a partir da distancia euclidiana
(define dijkstra (dij-from dist))

;; Transforma em grafo, dado a primeira questão, camadas intermediarias de artigos e as respostas
(define (to-graph question answers . list-articles)
  (set-node-neineighbors! question (first list-articles))

  (for ([articles1 list-articles]
        [articles2 (rest list-articles)])
    (for ([article1 articles1])
      (set-node-neineighbors! article1 articles2)))

  (for ([article (last list-articles)])
    (set-node-neineighbors! article answers))
  (for ([answer answers])
    (set-node-neineighbors! answer (list)))
  (append (list question) (apply append list-articles) answers))

;; Calcula a menor distância, o melhor artigo e a melhor resposta de um grafo com uma questão,
;; uma camada intermediaria de artigos e uma camada final de respostas
(define (get-distance-article-answer question articles answers [dist dist])
    (define graph (to-graph question answers articles))
    (define-values (distances previous) ((dij-from dist) graph question))
    (define min-distance
        (for/fold ([dist +inf.f])
            ([answer answers])
            (min dist (dict-ref distances answer))))
    (define best-answer
        (for/first ([answer answers]
            #:when (= min-distance (dict-ref distances answer)))
            answer))
    (define best-article (dict-ref previous best-answer))
    (values min-distance best-article best-answer))
