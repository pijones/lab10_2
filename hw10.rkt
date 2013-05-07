;Homework 10
;Laura Watiker
;Paul Jones

;exercise 1
(define length1
      (lambda (L) 
           (cond 
                 [(null? L) 0] 
                 [else (add1 (length1 (cdr L)))])))

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
  (lambda (l acc)
    (cond
      [(or (null? l) (null? (cdr l))) acc]
      [else (make-palindrome-acc )])))