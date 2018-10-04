#lang racket

(require math/matrix)
(require "../tfidf/tfidf.rkt")

(provide 
 norm
 dif
 dist
 scalar-prod
 cos-dist
 get-dist-miha
 dist-Mihalcea
 )
 
 (define (get-dist-miha tf tfidf)
  (lambda (u v) (dist-Mihalcea u v tf tfidf)))

(define (dist-Mihalcea T1 T2 tf tfidf-question)
  ;(display ".... 0. compute row ...." (current-error-port))
  (define T1-row (find-vector-row T1 tfidf-question))
  (define T2-row (find-vector-row T2 tfidf-question))

  ;(display ".... 1. compute idf-T1, idf-T2 ...." (current-error-port))
  ;given two documents T1, T2 - idf-T1, idf-T2 calculate Σ(idf[w]; w ∈ T1), Σ(idf[w]; w ∈ T2)
  (define idf-T1 (compute-idf T1 T1-row tf))
  (define idf-T2 (compute-idf T2 T2-row tf))

  ;(display ".... 2. compute idf-vec ...." (current-error-port))
  ;calculates idf-vec := [idf(w1), idf(w2), ..., idf(wn)]
  (define idf-vec (get-idf-vec tf))
  ;(display ".... 2a. - dim(idf-vec) = #cols?  ...." (current-error-port))
  ;(display (equal? (vector-length idf-vec) (vector-length (vector-ref tf 0))) (current-error-port))
  
  (display ".... compute distance ...." (current-error-port))
  ;computes the distance:
  (compute-miha-distance T1 T2 tf idf-vec idf-T1 idf-T2)
  )


;for two documents T1, T2:
;miha-dist := 0.5*(+ (/ Σ(maxSim(w,T2)*idf[w]; w ∈ T1)*Σ(idf[w]; w ∈ T1)) (/ Σ(maxSim(w,T1)*idf[w]; w ∈ T2)*Σ(idf[w]; w ∈ T2)) ))
(define (compute-miha-distance T1 T2 tf idf-vec idf-T1 idf-T2)
  ;(display ".... 4a. in compute-miha-distance ...." (current-error-port))
  (/ 2 (+ (/ (int-sim T1 T2 tf idf-vec) idf-T1)
          (/ (int-sim T2 T1 tf idf-vec) idf-T2))))



;calculates Σ(maxSim(w, U)*idf[w]; w in T)
(define (int-sim T U tf idf-vec)
  ;(display ".... 4b. in int-sim  ...." (current-error-port))
  (define m (vector-length tf))
  (define n (vector-length (vector-ref tf 0)))
  
  (define vec-T
    (for/vector ([i n])
      (if (not (zero? (vector-ref T i))) i 0)))
  (define vec-U
    (for/vector ([i n])
      (if (not (zero? (vector-ref U i))) i 0)))
  
  (for/sum ([i m])
    (* (sim-set-word U i tf m n vec-U) (vector-ref idf-vec i))))



;calculates maxSim(w, U) - the max similarity between a word and a document U
(define (sim-set-word U word-index tf m n vec-U)
  ;(display ".... 4c. in sim-set-word ...." (current-error-port))
  (define wordsU 0)
  ;(display ".... define vec-indices-U ...." (current-error-port))
  
  ;U = [0.13 0.01 0 0 0.35 ... 0.02 0] --> vec-indices-U = [1 2 0 0 5 ... n-2 n-1]
  (define vec-indices-U
    (for/vector ([i n])
      (if (not (zero? (vector-ref U i))) i 0)))

  ;(display ".... define vec ...." (current-error-port))
  ;reduces the need for (heavy) calculations of word-to-word similarity - 
  (define vec
    (for/vector ([index vec-indices-U])
      (if (zero? (vector-ref vec-indices-U index)) 0 (sim-word-word index word-index tf))))
  ;(display ".... (apply max vec) ...." (current-error-port))
  (apply max (vector->list vec)))



;calculates word-to-word similarity based on pmr-ir measure
(define (sim-word-word index-w1 index-w2 tf)
  ;(display ".... 4d. in sim-word-word ...." (current-error-port))
  ;(display ".... 4d. initial defines ...." (current-error-port))
  (define m (vector-length tf))
  (define favorable1 0)
  (define favorable2 0)
  (define favorable12 0)

  ;(display ".... 4d. for loops ...." (current-error-port))
  ;counts the number of documents where w1, w2 occur/co-occur and probabilities
  (for ([row tf])
    ;if w1 occurs in row
    (when (not (zero? (vector-ref row index-w1))) (set! favorable1 (+ favorable1 1)))
    ;if w2 occurs in row
    (when (not (zero? (vector-ref row index-w2))) (set! favorable2 (+ favorable2 1)))
    ;if w1 and w2 co-occur in row
    (when (and (not (zero? (vector-ref row index-w1))) (not (zero? (vector-ref row index-w2)))) (set! favorable12 (+ favorable12 1))))
  ;(display ".... 4d. returning pmi-ir ...." (current-error-port))
  
  ;(from article); With p(wi) approximated as hits(w1)/WebSize, the following PMI-IR measure is obtained:
  ;(display ".... favorable1: " (current-error-port))
  ;(display favorable1 (current-error-port))
  ;(display ".... favorable2: " (current-error-port))
  ;(display favorable2 (current-error-port))
  ;(display ".... favorable12: " (current-error-port))
  ;(display favorable12 (current-error-port))
  (if (zero? favorable12) 0 (log (* (/ favorable12 (* favorable1 favorable2)) m) 2)))




;----------------
;(define (get-prob-inter index-w1 index-w2 tf m)
;  (define col-1 (get-column tf index-w1))
;  (define col-2 (get-column tf index-w2))
;  (define favourable (length (intersection col-1 col-2)))
;  (/ favourable m)
;  )

;----------------
(define (get-column tf col)
  (define m (length tf))
  (for/vector ([i m])
    (get-matrix-pos tf i col)))

(define (intersection u v)
  (define dummy (build-list (vector-length u) values))
  (define u1 (map (lambda (x y) (if (zero? x) y 0)) u dummy))
  (define v1 (map (lambda (x y) (if (zero? x) y 0)) v dummy))
  (define u2 (list->set (vector->list u1)))
  (define v2 (list->set (vector->list v1)))
  (set-intersect u2 v2))


(define (get-row tf-question row)
  (define n (length (vector-ref tf-question 0)))
  (for/vector ([i n])
    (get-matrix-pos tf-question row i)))

(define (get-prob-vec tf)
  (define n (vector-length (vector-ref tf 0)))
  (for/vector ([i n])
    (prob-word i tf)))

(define (prob-word word-index mat)
  (define m (vector-length mat))
  (define total (sum-matrix mat))
  (define favorable
    (for/sum ([i m])
      (get-matrix-pos mat i word-index)))
  (/ favorable total))

(define (find-vector-row v M)
  (let/ec return
    (define row 0)
    (define return-row 0)
    (define counter 0)
    (for ([w M])
      (set! counter (+ counter 1))
      (cond
        [(equal? w v) (return row)]
        [else (set! row (+ row 1))]
        ))))

(define (get-matrix-pos matrix indexX indexY)
  (let/ec return
  (return (vector-ref (vector-ref matrix indexX) indexY))))
;tf --> 
(define (get-idf-vec tf)
  (define n (vector-length (vector-ref tf 0)))
  (for/vector ([i n])
    (compute-idf-col tf i)))

;document T -> Σ(idf[w]; w ∈ T)
(define (compute-idf T T-row tf)
  (define len (vector-length T))
  (for/sum ([i (in-range len)])
    (if (not (zero? (vector-ref T i))) (compute-idf-col tf i)  0)
    ))

;index-w --> idf[w] for a word w
(define (compute-idf-col tf-question colNumber)
  (define idf-col (get-idf-freq-col tf-question colNumber))
  (cond
    ;if (zero? idf-col) then word was added to corpus but not computed (?) --> PARSE ERROR (?)
    [(zero? idf-col) 0]
    [else (log idf-col 2)]
    ))

;idf(w) = log(idf-freq) = log(total #doc/#docs term w in it) = log(m/n(w))
(define (get-idf-freq-col tf-question colNumber)
  (define m (vector-length tf-question))
  ;m = no. de linhas tf-question
  (/ 1 (for/sum ([s (in-range m)])
    (cond
      ;tf(s, colNumber) = 0 ->
      [(zero? (get-matrix-pos tf-question s colNumber)) 0]
      [else (/ 1 m)])))
  )



(define (sum-vector vec)
  (define len (vector-length vec))
  (for/sum ([i vec]) i))

(define (sum-matrix mat)
  (define m (vector-length mat))
  (for/sum ([i mat])
    (sum-vector i)))


















(define (norm vec)
  (sqrt (for/fold ([sum 0.0])
                  ([x (in-vector vec)])
          (+ sum (* x x)))))

;calcula a diferença entre cada elemento (v1-v2).
(define (dif v1 v2)
  (vector-map - v1 v2))

;define valor da distancia do vetor. 
(define (dist v1 v2)
  (norm (dif v1 v2)))

(define (scalar-prod v1 v2)
  (for/sum 
      ([i v1] [j v2])
    (* i j)))

(define (cos-dist v1 v2)
  (- 1 (/
        (scalar-prod v1 v2)
        (sqrt (*
               (scalar-prod v1 v1)
               (scalar-prod v2 v2))))))

(define (de-normalize-rows mat)
  (define len (vector-length mat))
  (for/vector ([row mat])
    (de-normalize row)))

(define (mul-list lst x)
  (map (lambda (n) (* x n)) lst))

(define (div-list lst x)
  (map (lambda (n) (/ n x)) lst))

(define (round-list lst)
  (map (lambda (n) (round n)) lst))

(define (positive-min v)
  (define l (vector->list v))
  (for/fold ([min +inf.0]) ([x l])
    (if (and (> min x) (> x 0)) x min)))

(define (de-normalize v)
  (define l (vector->list v))
  (define minm (positive-min v))
  (define l2 (div-list l minm))
  (define l3 (round-list l2))
  (list->vector l3))

(define (vector-diff-list lst1 lst2 fun)
  (map (lambda (x s) (fun x s)) lst1 lst2))

(define (add-list lst1 lst2 fun)
  (for/sum 
      ([i lst1] [j lst2])
    (fun i j)))


;norma-p, x>0
(define (p-sum x p)
  (for/sum ([i x]) (expt (list-ref x (- i 1)) p)))

(define (p-norm x p)
  (expt (p-sum x p) (/ 1 p)))

(define (p-grad x0 p)
  (map (lambda (x)
         (/ (* x (expt (abs x) (- p 2))) (expt (p-norm x0 p) (- p 1))))
       x0))







;simulador cos(x,x0) = dot(x,x0)/((|x|^2)*(|x0|^2))
(define (simf1 v1 v2)
  (/ (scalar-prod v1 v2)
     (sqrt (*
            (scalar-prod v1 v1)
            (scalar-prod v2 v2)))
     )
  )

(define (simf2 v1 v2)
  (scalar-prod (- v1 v2) (- v1 v2)))

(define (simgradf2 v1 v2)
  (mul-list (add-list v1 (mul-list v2 (- 0 1))) 2))

(define (subtract xs ys)
  (if (empty? ys)
      xs
      (subtract (remove (first ys) xs) (rest ys))))




;simulador gradiente da funcao f(x) := cos(x,x0) - aqui queremos x0 fixo para rodar o metodo do gradiente
(define (simgradf1 v1 v2)
  (mul-list v1 (/ (scalar-prod v1 (- v1 v2)) (* (expt (sqrt (scalar-prod v1 v1)) 3) (sqrt (scalar-prod v2 v2))))))


;note que toda norma é convexa ||tx+(1-t)y|| <= t||x|| + (1-t)||y||
;além disso algumas normas sao diferenciaveis longe dos eixos!
;ex: normas Lp

;podemos portanto formular, para uma dada lei x0, o problema como
;um problema de otimizacao: min ||x-x*||, x ∈ R^n, onde
;-x0 representa o vetor pergunta (iteracao 0)
;-x* representa a lei
;Podemos utilizar, por exemplo, o metodo do gradiente 
;e calcular quantas iteracoes precisamos para ir x0--->x*
;se considerarmos que uma resposta valida deve estar dentro
;de uma bola em torno de x*
;----note: x* é um minimo para a funcao em questao 


;metodo do gradiente, t = stepSize, retorna o numero de passos
;x - posição inicial do método (pergunta)
;x0 - posição final (lei)
;queremos minimizar f(x) := ||x-x0||_p, só terá minimo quando x = x0
;queremos ver em quantos passos f(x0) converge a zero 
(define (grad-method-dist x x0 tolerance t k simgrad simf p)
  (let/ec return
    (let ([x (if (= k 0) (vector-diff-list x (make-vector (length x) 0.00001) +) x)])
      ;tirando os zeros do vetor - se xi= 0 ----> derivada +inf
    
      (define y (vector-diff-list x x0))
      (define f (simf y p))
      (define gradf (simgrad y p))
      (define xk (- x (mul-list (simgrad y p) (- 0 t))))
      (add1 k)
      (cond
        [(and (< (norm (simgrad xk p)) tolerance) (< k 100000)) (return k)]
        [(< k 100000) (return (grad-method-dist xk x0 tolerance t k simgrad simf p))]
        ;exceção se não convergir :(, retorne +inf.0
        [(> k 100000) (return +inf.0)]
        ))))
