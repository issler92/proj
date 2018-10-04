#lang racket

(require rackunit)

(require "dist.rkt")

(check-equal? (dif (vector) (vector)) (vector) "dif of vector with no size")
(check-equal? (dif (vector 1.) (vector 1.)) (vector 0.) "dif must be vector 0")
(check-equal? (dif (vector 1. 2.) (vector 1. 1.2)) (vector 0. 0.8) "dif of size 2 not equal")

(check-equal? (dist (vector 0. 3.) (vector 4. 0.)) 5.  "dif of pitagora theorem not equal")

(check-equal? (scalar-prod (vector) (vector)) 0)
(check-equal? (scalar-prod (vector 2 3) (vector 100 1000)) 3200)

(check-equal? (cos-dist (vector 1) (vector 3)) 0)
(check-equal? (cos-dist (vector 1 0) (vector 3 0)) 0)
(check-equal? (cos-dist (vector 0 1) (vector 3 0)) 1)
(check-equal? (cos-dist (vector 2 0) (vector 3 4)) 2/5)