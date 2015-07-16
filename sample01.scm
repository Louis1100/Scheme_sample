; Hello world as a variable
(define vhello "Hello World")


; Hello world as a function
(define fhello (lambda () "Hello World"))


; hello with name
(define hello
  (lambda (name)
   (string-append "Hello " name "!")))


; sum of three numbers
(define sum3
  (lambda (a b c)
   (+ a b c)))

; ３つの数字を足す関数
(define three-args+
  (lambda (a b c . d)
    (list a b c d)))


(define (inc x)
  (+ x 1))


(define (dec x)
  (- x 1))


(define (sum-gp a0 r n)
  (* a0
     (if (= r 1)
         n
         (/ (- 1 (expt r n)) (- 1 r)))))   ; !!
;; 等差数列の和はa0 * (1 - r**n) / (1 - r)
;; ただし公差1=> a0 * n


; cond節は複合if文を表記
(define (fee age)
  (cond
    ((or (<= age 3) (>= age 65)) 0)
    ((<= 4 age 6) 50)
    ((<= 7 age 12) 100)
    ((<= 13 age 15) 150)
    ((<= 16 age 18) 180)
    (else 200)))


(define (check score)
  (cond
    ((<= 80 score) "A")
    ((<= 60 score 79) "B")
    ((<= 40 score 59) "C")
    (else "D")))

; beginの使い方
(define (foo)
  (begin
    (display "hello world.")
    (newline)
    (display "I love Scheme.")
    (newline)
    'done))
