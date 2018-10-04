#lang racket

(require rackunit)
(require racket/block)

(require "graph.rkt"
         "../data-structures.rkt")

(block
    (define node1 (node "n1" (vector 0)))
    (define graph (list node1))
    (define-values (distances previous) (dijkstra graph node1))
    (check-equal? (dict-ref distances node1) 0))

(block
    (define node1 (node "n1" (vector 0)))
    (define node2 (node "n2" (vector 1)))
    (set-node-neineighbors! node2 (list node1))
    (set-node-neineighbors! node1 (list node2))

    (define graph (list node1 node2))
    (define-values (distances previous) (dijkstra graph node1))
    (check-equal? (dict-ref distances node1) 0)
    (check-equal? (dict-ref distances node2) 1.)
    (check-equal? (dict-ref previous node2) node1)
)

(block
    (define question (node "q" (vector 0)))
    
    (define art1 (node "art1" (vector 1)))
    (define art2 (node "art2" (vector -2)))
    (define articles1 (list art1 art2))

    (define art3 (node "art3" (vector 1)))
    (define art4 (node "art4" (vector -2)))
    (define articles2 (list art3 art4))

    (define ans1 (node "ans1" (vector -3)))
    (define ans2 (node "ans2" (vector 7)))
    (define answers (list ans1 ans2))

    (define graph (to-graph question answers articles1 articles2))
    (check-equal? graph (list question art1 art2 art3 art4 ans1 ans2)))


(block
    (define question (node "q" (vector 0)))
    
    (define art1 (node "art1" (vector 1)))
    (define art2 (node "art2" (vector -2)))
    (define articles (list art1 art2))

    (define ans1 (node "ans1" (vector -3)))
    (define ans2 (node "ans2" (vector 7)))
    (define answers (list ans1 ans2))

    (define graph (to-graph question answers articles))
    (check-equal? graph (list question art1 art2 ans1 ans2))
    (check-equal? (node-neineighbors question) articles)
    (check-equal? (node-neineighbors art1) answers)
    (check-equal? (node-neineighbors art2) answers)
    (check-equal? (node-neineighbors ans2) (list))

    (define-values (dists previous) (dijkstra graph question))
    (check-equal? (dict-ref dists ans1) 3.)
    (check-equal? (dict-ref dists ans2) 7.)
    (check-equal? (dict-ref previous ans1) art2)

    (define-values (min-dist best-art best-ans) (get-distance-article-answer question articles answers))
    (check-equal? min-dist 3.)
    (check-equal? best-ans ans1)
    (check-equal? best-art art2))