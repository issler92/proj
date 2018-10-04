#lang racket

;(define (max-sim index-w T2 dist)
  ;T2 é vec, index-w é inteiro
  ;(define T2-len (length T2))
 ;; (define w-vector (make-vector T2-len 0))
;  (set! (vector-ref w-vector index-w) 1) 
  ;(define cols (T2-len))
 ;; (for/fold ([sim 0])
  ;          ([i T2-len])
  ;;  
 ;   (max sim ))
 ;   )
 ;   (define j (make-vector (length T2) 0))
;    (set! (vector-ref (make-vector (length T2) 0) i) 1)
;    (dist w T2) 
;    )
;  )
;;
;(define (function vec)
;  (for/sum ([i vec])
  ;  (define j (* i i))
 ;   j
 ;   )
 ; )


;(define (max-sim w T2 dist)
;  (define cols (length T2))
;  (for ([i cols])
 ;   
;    )
;  )

(define (aux n) (if (equal? n (- 3 1)) 1 0))
(build-vector 10 (lambda (x) (aux x)))
