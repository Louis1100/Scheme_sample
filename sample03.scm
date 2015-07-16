; (define 99 80)  ==>> return 19
(define minus
  (lambda (n m)
    (cond
      ((zero? m) n)
	  (else (sub1 (minus n (sub1 m)))))))


; (define 10 30) ==>> return 40 
(define plus
  (lambda (n m)
    (cond
      ((zero? m) n)
    (else (add1 (plus n (sub1 m)))))))


(define add1
  (lambda (n)
    (+ n 1)))


(define sub1
  (lambda (n)
    (- n 1)))


(define **
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else (plus n (** n (sub1 m)))))))

;(define tup+
;  (lambda (tup1 tup2)
;    (cond
;      ((and (null? tup1) (null? tup2)) (quote ()))
;      (else
;       (cons (plus (car tup1) (car tup2))
;             (tup+
;              (cdr tup1) (cdr tup2)))))))
; 下が改良版
; (tup+ (list 1 2 3 4 5) (list 2 3 4 5 6)) ==>> return (3 5 7 9 11)


(define tup+
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else
       (cons (plus (car tup1) (car tup2))
             (tup+
              (cdr tup1) (cdr tup2)))))))

; (>> 4 2) ==>> return #t(True)
(define >>
  (lambda (n m)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else (>> (sub1 n) (sub1 m))))))


(define <<
  (lambda (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else (<< (sub1 n) (sub1 m))))))


(define ==
  (lambda (n m)
    (cond
      ((<< n m) #f)
      ((>> n m) #t)
      (else #t))))



(define **
  (lambda (n m)
    (cond
      ((zero? m) 1)
      (else
       (* n (** n (sub1 m)))))))


(define //
  (lambda (n m)
    (cond
      ((< n m) 0)
      (else (add1 (// (- n m) m))))))

;　リストの中の要素数を調べる関数
(define len
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (len (cdr lat)))))))


; リスト内でnで指定した番号のアトムを返す関数
(define pick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      (else (pick (sub1 n) (cdr lat))))))

;　リスト内でnで指定したアトムを削除し結果をリストで返す関数
(define rempick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (cdr lat))
      (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))


; 指定したリスト内の数を削除する関数
(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
              ((number? (car lat)) (no-nums (cdr lat)))
              (else
               (cons (car lat) (no-nums (cdr lat)))))))))

;  指定したリストを数だけにする関数
(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
              ((number? (car lat))
               (cons (car lat) (all-nums (cdr lat))))
              (else (all-nums (cdr lat))))))))

; 数の等しさを表す=とアトムの同一性を表すeq?を一つの関数に
(define eqan?
  (lambda (a1 a2)
    (cond
      ((and (number? a1) (number? a2))
       (= a1 a2))
      ((or (number? a1) (number? a2)) #f)
      (else (eq? a1 a2)))))

; 指定したアトムを数える関数
(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
    (else
     (cond
       ((eq? (car lat) a)
        (add1 (occur a (cdr lat))))
       (else (occur a (cdr lat))))))))


;(define one?
;  (lambda (n)
;    (cond
;      ((zero? n) #f)
;      (else (zero? (sub1 n))))))
; 下が改良版
; nが1であるときに#tとなる関数

(define one?
  (lambda (n)
    (= n 1)))





