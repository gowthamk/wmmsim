#lang s-exp rosette

(define (sudoku-board)
  (for/list ([i 9])
    (for/list ([j 9])
      (define-symbolic* k number?)
      (assert (and (> k 0) (< k 10)))
      k)))

(define-syntax-rule (define-sudoku-board brd)
  (define brd (sudoku-board)))

(define (cellAt brd r c)
  (list-ref (list-ref brd r) c))

(define (assert-distinct syms)
  (for ([i (length syms)])
    (for ([j (in-range (+ i 1) (length syms) 1)])
      (assert (not (eq? (list-ref syms i)
                      (list-ref syms j)))))))
    
(define (get-ith-column brd i)
  (map (lambda (row) (list-ref row i)) brd))

(define (n-rows brd)
  (length brd))

(define (n-cols brd)
  (if (> (length brd) 0)
      (length (first brd))
      0))

(define (transpose brd)
  (for/list ([i (length brd)])
    (get-ith-column brd i)))

(define (transpose3X3 brd)
  (cond
    ((or (< (n-rows brd) 3) (< (n-cols brd) 3)) '())
    (else 
     (let* ([first3X3 (map (lambda (row) (take row 3)) 
                        (take brd 3))]
         [rest6X3 (map (lambda (row) (take row 3))
                       (drop brd 3))]
         [rest9X6 (map (lambda (row) (drop row 3))
                       brd)])
       (cons (flatten first3X3) 
                  (append (transpose3X3 rest6X3)
                          (transpose3X3 rest9X6)))))))

(define (assert-row-uniqueness brd)
  (for-each (lambda (row) (assert-distinct row)) brd))

(define (assert-col-uniqueness brd)
  (assert-row-uniqueness (transpose brd)))

(define (assert-3X3-uniqueness brd)
  (assert-row-uniqueness (transpose3X3 brd)))

;; The demo
(define-sudoku-board brd)
(assert-row-uniqueness brd)
(assert-col-uniqueness brd)
(assert-3X3-uniqueness brd)
(define sol (solve #t))
(display sol)
(print (evaluate brd sol))