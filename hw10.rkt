;Homework 10
;Laura Watiker
;Paul Jones

;We have adhered to the Honor Code in completiong of this assignment.

;(atom? returns #t if argument x is an atom and #f if it isnt'
(define atom?
  (lambda (x)
    (cond
      [(null? x) #f]
      [(pair? x) #f]
      [else #t])))

;exercise 1
(define length1
      (lambda (L) 
           (cond 
                 [(null? L) 0] 
                 [else (add1 (length1 (cdr L)))])))

;Starting accumulator must be 0
(define length-acc
  (lambda (L acc)
    (cond
      [(null? L) acc]
      [else (length-acc (cdr L) (add1 acc))])))

;exercise 2
(define make-palindrome
    (lambda (l)
      (cond
 	  [(or (null? l) (null? (cdr l))) l]
          [else (cons (car l) (append (make-palindrome (cdr l)) (list (car l))))])))
		
(define make-palindrome-acc
  (lambda (l acc1 acc2)
        (if (null? (cdr l))
            (append acc1 l acc2)
            (make-palindrome-acc (cdr l) (append acc1 (list (car l))) (append (list (car l)) acc2)))))

;exercise 3
(define flatten
    (lambda (l)      
		(if (null? l) ()
        (if (atom? l) (list l)
			            (append (flatten (car l)) (flatten (cdr l)))))))  

(define flatten-acc
  (lambda (l acc)
    (if (null? l) acc
        (if (atom? l) (append acc (list l))
            (flatten-acc (cdr l) (flatten-acc (car l) acc))))))

;exercise 4
(define length-k
  (lambda (l k)
    (if (null? l) (k 0)
        (length-k (cdr l) (lambda (y) (k (+ 1 y)))))))

;exercise 5
(define make-palindrome-k
  (lambda (l k)
    (if (or (null? l) (null? (cdr l)))
        (k l)
        (make-palindrome-k (cdr l)(lambda (y) (k (append  (list (car l)) y (list (car l)) )))))))

;exercise 6
(define flatten-k
  (lambda (l k)
    (if (null? l) (k l)
        (if (atom? l) (k (list l))
           (flatten-k (car l) (lambda (y) (flatten-k (cdr l) (lambda (x) (k (append y x))))))))))

;exercise 7
(define pairone
    (lambda (a l)
       (if (null? l) ()
           (cons (list a (car l)) (pairone a (cdr l))))))
    

(define pairall    
		(lambda (l1 l2)       
			(if (null? l1) ()
           (append (pairone (car l1) l2) (pairall (cdr l1) l2)))))  

(define pairone-k
  (lambda (a l k)
    (if (null? l) (k l)
        (pairone-k a (cdr l) (lambda (y) (k (cons (list a (car l)) y)))))))

(define pairall-k
  (lambda (l1 l2 k)
    (if (null? l1) (k ())
        (pairall-k (cdr l1) l2 (lambda (y) (k (append (pairone-k (car l1) l2 (lambda (y) y)) y)))))))
    


    