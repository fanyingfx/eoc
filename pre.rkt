#lang racket

(require racket/fixnum)
(require "utilities.rkt")
; (struct Int (value))
; (struct Prim (op args))
(define eight (Int 8))
(define neg-eight (Prim '- (list eight)))
(define rd (Prim 'read '()))
(define ast1_1 (Prim '+ (list rd neg-eight)))

(struct Read())
(struct Add (left right))
; (struct Program (info value))
(define (leaf arith)
  (match arith
    [(Int n) #t]
    [(Prim 'read '()) #t]
    [(Prim '- (list e1)) #f]
    [(Prim '+ (list e1 e2)) #f]
    [(Prim '- (list e1 e2)) #f]))
(define (is_exp ast)
  (match ast
    [(Int n) #t]
    [(Prim 'read '()) #t]
    [(Prim '- (list e)) (is_exp e)]
    [(Prim '+ (list e1 e2)) (and (is_exp e1) (is_exp e2))]
    [(Prim '- (list e1 e2)) (and (is_exp e1) (is_exp e2))]
    [(Prim '* (list e1 e2)) (and (is_exp e1) (is_exp e2))]
    [(Prim '+ (list e)) #t]
    [else #f]
    )
  )

(define (is_Lint ast)
  (match ast
    [(Program '() e)  (is_exp e)]
    [else #f]
    )
  )

(define (interp_exp e)
  (match e
    [(Int n) n]
    [(Prim 'read '())
     (define r (read))
     (cond [(fixnum? r) r]
           [else (error 'interp_exp "read expected an integer: ~v" r)])
     ]
    [(Prim '- (list e))
     (define v (interp_exp e))
     (fx- 0 v)
     ]
    [(Prim '+ (list e1 e2))
     (define v1 (interp_exp e1))
     (define v2 (interp_exp e2))
     (fx+ v1 v2)
     ]
    [(Prim '- (list e1 e2))
     (define v1 (interp_exp e1))
     (define v2 (interp_exp e2))
     (fx- v1 v2)
     ]
    )

  )
(define (interp_Lint p)
  (match p
    [(Program '() e) (interp_exp e)]
    ))
; (define n 999999999999999999)

; (is_Lint (Program '() ast1_1))
; (is_Lint (Program '() (Prim '* (list (Prim 'read '())
;                                      (Prim '+ (list (Int 8)) )))))

; (interp_Lint (Program '() ast1_1))

(define (pe_neg r)
  (match r
    [(Int n) (Int (fx- 0 n))]
    [else (Prim '- (list r))]))
(define (pe_add r1 r2)
  (match* (r1 r2)
    [((Int n1) (Int n2)) (Int(fx+ n1 n2))]
    [(_ _) (Prim '+ (list r1 r2))]))
(define (pe_sub r1 r2)
  (match* (r1 r2)
    [((Int n1) (Int n2)) (Int(fx- n1 n2))]
    [(_ _) (Prim '- (list r1 r2))]
    ))

(define (pe_exp e)
  (match e
    [(Int n) (Int n)]
    [(Prim 'read '()) (Prim 'read '())]
    [(Prim '- (list e1)) (pe_neg (pe_exp e1))]
    [(Prim '+ (list e1 e2))(pe_add (pe_exp e1) (pe_exp e2) )]
    [(Prim '- (list e1 e2))(pe_sub (pe_exp e1) (pe_exp e2) )]
    ))
(define (pe_Lint p)
  (match p
    [(Program '() e)(Program '() (pe_exp e))]))

(define (test_pe p)
  (assert "testing pe_Lint"
          (equal? (interp_Lint p) (interp_Lint (pe_Lint p)))))
(test_pe (parse-program `(program () (+ 10 (- (+ 5 3))))))
(test_pe (parse-program `(program () (+ 1 (+ 3 1)))))
(test_pe (parse-program `(program () (- (+ 3 (- 5))))))