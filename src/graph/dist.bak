#lang racket

(require math/matrix)

(provide 
 norm
 dif
 dist
 scalar-prod
 cos-dist
 )


;obs: Para calculo de distancia, foi usada distancia euclideana.
;calcula a norma de vetor.
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

(define (mul-list lst x)
  (map (lambda (n) (* x n)) lst))


(define (vector-diff-list lst1 lst2 fun)
  (map (lambda (x s) (fun x s)) lst1 lst2))

(define (add-list lst1 lst2)
  (for/sum 
      ([i lst1] [j lst2])
    (+ i j)))


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
