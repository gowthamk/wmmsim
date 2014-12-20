#lang s-exp rosette

(struct id-com (tid cid com))

(define (map2 f l1 l2) 
  (cond 
    ((and (null? l1) (null? l2)) '())
    (else (cons (f (list (first l1) (first l2))) 
                (map2 f (rest l1) (rest l2))))))

(define (id-annotate-prog prog) 
  (map2 (lambda (tidtd) 
          (let* ([tid (first tidtd)]
                 [td (second tidtd)])
            (list tid (map2 (lambda (cidc) 
                    ;(id-com tid (first cidc) (second cidc))
                    (list (list tid (first cidc)) (second cidc)))
                  (build-list (length td) values)
                  td))))
        (build-list (length prog) values)
        prog))

(define (count-stores-in-td td)
  foldl (lambda (idcom acc)
          (if (eq? (first (second idcom)) 'STORE)
              (+ 1 acc)
              acc)) 0 td)
(define (count-all-stores idprog)
  (foldl (lambda (idtd acc)
           (+ acc (count-stores-in-td (second idtd)))) 
         0 idprog))

(define (comTagOf idcom) (first (second idcom)))

(define (filter-stores-in-td td)
  (filter (lambda (idcom)
            (eq? (comTagOf idcom) 'STORE))
          td))
(define (filter-stores idprog) 
  (filter-not null? 
              (map (lambda (idtd) 
                     (list (first idtd)
                           (filter-stores-in-td (second idtd)))) 
                   idprog)))

(define (nondet-mix idtds cnt)
  (cond
    ((eq? cnt 0) '())
    (else
     (define-symbolic* k number?)
     (let* ([idtd (list-ref idtds k)]
            [id (first idtd)]
            [td (second idtd)]
            [hd-idcom (first td)]
            [newidtd (list id (rest td))]
            [newidtds (filter-not null? (map (lambda (idtd) 
                             (if (eq? (first idtd) id)
                                 newidtd
                                 idtd)) idtds))]
            [tl-idcoms (nondet-mix newidtds (- cnt 1))])
       (cons hd-idcom tl-idcoms)))))

(define (count-coms idprog)
  (foldl (lambda (idtd acc) 
           (+ (length (second idtd)) acc)) 0 idprog))

(define (generate-exn my-idtd idprog)
  (let* ([my-id (first my-idtd)]
         [others (filter-not 
                  (lambda (idtd) (eq? (first idtd) my-id)) 
                  idprog)]
         [other-idtds (filter-stores others)]
         ;[x (display other-idtds)]
         [cnt (+ (length (second my-idtd)) 
                 (count-coms other-idtds))]
         [myexn (nondet-mix (cons my-idtd other-idtds) cnt)])
    myexn))

(define (generate-exns idprog)
  (map (lambda (myidtd) 
         (generate-exn myidtd idprog)) idprog))

; All shared vars are initialized to zero
(define (empty)
  '())

(define (update st/env x v)
  (printf "update: ~a -> ~a\n" x v)
  (cons (list x v) st/env))

(define (lookup-st st x)
  (cond 
   ((null? st) 0)
   (else (if (eq? (first (first st)) x)
             (second (first st))
             (lookup-st (rest st) x)))))

(define (lookup-env env x)
  (let* ([matches (memf (lambda (yv) 
                          (eq? (first yv) x)) 
                        env)])
    (cond 
       ((not matches) -1)
       (else (second (first matches))))))

(define (interpret-com com st env)
  (cond
    ((eq? (first com) 'LOAD) ;LOAD 
     (list st (update env (third com) 
                      (lookup-st st (second com)))))
    ((eq? (first com) 'STORE) ;STORE
     (list (update st (second com) (third com))
           env))
    (else (error "invalid syntax"))))

(define (interpret-exn tdexn env)
  (let* ([display tdexn]
         [stenv (foldl (lambda (idcom acc)
                         (interpret-com (second idcom) (first acc) (second acc))) 
                       (list (empty) env) tdexn)])
    (second stenv)))

(define (interpret-exns exns)
  (let* ([x (display (length exns))]
         ;[x (display exns)]
         ;[x (foldl (lambda (exn k)
         ;            (display exn)) '() exns)]
         [env (foldl (lambda (exn env) 
                       (interpret-exn exn env)) 
                     (empty) exns)])
    env))

; main
(define (doit prog)
  (let* ([idprog (id-annotate-prog prog)]
         [exns (generate-exns idprog)]
         [env (interpret-exns exns)])
    (list exns env)))

; demo1
(define myprog (list (list (list 'STORE 21 1) (list 'LOAD 22 31))
                           (list (list 'STORE 22 1) (list 'LOAD 21 32))
                          ))
(define x (doit myprog))
(define myexns (first x))
(define myenv (second x))
(assert (eq? (lookup-env myenv 31) 0))
(assert (eq? (lookup-env myenv 32) 0))
(define M (solve #t))
(printf "\n\n\n\n\nmyexns:\n")
(print (evaluate myexns M))

; demo2
(set! myprog (list (list (list 'STORE 21 1))
                      (list (list 'STORE 22 1))
                      (list (list 'LOAD 21 31) (list 'LOAD 22 32))
                      (list (list 'LOAD 21 33) (list 'LOAD 22 34))))
(set! x (doit myprog))
(set! myexns (first x))
(set! myenv (second x))
(assert (eq? (lookup-env myenv 31) 1))
(assert (eq? (lookup-env myenv 32) 0))
(assert (eq? (lookup-env myenv 33) 0))
(assert (eq? (lookup-env myenv 34) 1))
(set! M (solve #t))
(printf "\n\n\n\n\nmyexns:\n")
(print (evaluate myexns M))