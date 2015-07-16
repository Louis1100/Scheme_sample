; 指定したxがアトムかを確かめる関数
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))


; 複合リスト内の指定したアトムを消す関数
(define rember*
  (lambda (a l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l))
      (cond
        ((eq? (car l) a)
         (rember* a (cdr l)))
        (else (cons (car l)
                    (rember* a (cdr l))))))
      (else (cons (rember* a (car l))
			 (rember* a (cdr l)))))))



; insertRはoldがどこにあっても,oldの右にアトムnewを入れる関数
; insertR*はリストを含んだリストでもinsertRが動作するようにした関数
; ＊がついているものは全てリスト対応版。
(define insertR*
  (lambda (new old l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l))
       (cond
         ((eq? (car l) old)
          (cons old
                (cons new
                      (insertR* new old (cdr l)))))
       (else (cons (car l)
                   (insertR* new old (cdr l))))))
    (else (cons (insertR* new old (car l))
                (insertR* new old (cdr l)))))))
                

(define insertL*
  (lambda (new old l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l))
       (cond
         ((eq? (car l) old)
          (cons new
                (cons old
                      (insertL* new old (cdr l)))))
         (else (cons (car l) 
               (insertL* new old (cdr l))))))
       (else (cons (insertL* new old (car l))
                   (insertL* new old (cdr l)))))))
                
                
;occur*はS式のリストの中の指定したアトムを数える関数
(define occur*
  (lambda (a l)
    (cond
      ((null? l) 0)
      ((atom? (car l))
       (cond
         ((eq? (car l) a)
         (add1 (occur* a (cdr l))))
         (else (occur* a (cdr l)))))
         (else (+ (occur* a (car l))
                   (occur* a (cdr l)))))))


;subst*はS式の含むリストの中のoldアトムを全てnewアトムに置き換える関数
(define subst*
  (lambda (new old l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l))
       (cond
         ((eq? (car l) old)
          (cons new (subst* new old (cdr l))))
         (else 
          (cons (car l) (subst* new old (cdr l))))))
      (else 
       (cons (subst* new old (car l)) (subst* new old (cdr l)))))))




(define member*
  (lambda (a l)
    (cond
      ((null? l) nil)
      ((atom? (car l))
       (or (eq? (car l) a)
            (member* a (cdr l))))
       (else (or (member* a (car l))
                 (member* a (cdr l)))))))

; leftmostは一番左の位置にあるアトムを探す
; Pythonのstartwithみたいなもの
(define leftmost
  (lambda (l)
    (cond
      ((atom? (car l)) (car l))
       (else (leftmost (car l))))))


