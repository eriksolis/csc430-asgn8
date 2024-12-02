#lang typed/racket
(require typed/rackunit)


;; Define ExprC
(define-type ExprC (U NumC IdC StrC IfC LamC AppC))
(struct NumC ([n : Real]) #:transparent)
(struct IdC ([name : Symbol]) #:transparent)
(struct StrC ([contents : String]) #:transparent)
(struct IfC ([test : ExprC] [then : ExprC] [else : ExprC]) #:transparent)
(struct LamC ([args : (Listof Symbol)] [body : ExprC]) #:transparent)
(struct AppC ([fun : ExprC] [args : (Listof ExprC)]) #:transparent)

;; Define Value
(define-type Value (U NumV StrV BoolV CloV PrimV))
(struct NumV ([n : Real]) #:transparent)
(struct StrV ([contents : String]) #:transparent)
(struct BoolV ([b : Boolean]) #:transparent)
(struct CloV ([args : (Listof Symbol)] [body : ExprC] [env : Env]) #:transparent)
(struct PrimV ([op : Symbol]) #:transparent)

;; Define Environment
(define-type Env (Listof Binding))

(struct Binding ([name : Symbol] [val : Value]) #:transparent)
(define mt-env '())
(define extend-env cons)

(define top-env (list
                 (Binding 'true (BoolV #t))
                 (Binding 'false (BoolV #f))
                 (Binding '+ (PrimV '+))
                 (Binding '- (PrimV '-))
                 (Binding '* (PrimV '*))
                 (Binding '/ (PrimV '/))
                 (Binding '<= (PrimV '<=))
                 (Binding 'equal? (PrimV 'equal?)) 
                 (Binding 'error (PrimV 'error))
                 (Binding 'println (PrimV 'println))
                 (Binding 'read-num (PrimV 'read-num))
                 (Binding 'read-str (PrimV 'read-str))
                 (Binding 'seq (PrimV 'seq))
                 (Binding '++ (PrimV '++)))) 

;; accepts a Value and returns its corresponding String representation
(define (serialize [val : Value]) : String
  (match val
    [(NumV n) (~v n)]
    [(StrV s) (~v s)]
    [(BoolV b) (match b
                 [#true "true"]
                 [#false "false"])]
    [(CloV args body env) "#<procedure>"] 
    [(PrimV op) "#<primop>"]))


;; list of keywords not allowed to be used as identifier names
(define banned-ids (list 'bind 'if '=> '=))

;; takes in a symbol and checks if that symbol is a valid name for an identifier
(define (valid-id? [name : Symbol]) : Boolean
  (not (member name banned-ids)))

;; parses a given Sexp into an ExprC
(define (parse [s : Sexp]) : ExprC
  ;(printf "current tree: ~a\n" s) 
  (match s
    ;; parsing NumC
    [(? real? n) (NumC n)]
    
    ;; parsing idC
    [(? symbol? name) (cond 
                        [(valid-id? name) (IdC name)]
                        [else (error 'parse "AAQZ received an invalid IdC, got ~e" name)])] 
    
    ;; parsing StrC
    [(? string? n) (StrC n)]

    ;;parsing ifC
    [(list 'if test then else)
     (IfC (parse test) (parse then) (parse else))]


    ;; parsing bind
    [(list 'bind (list ids '= vals) ... body)  
     (cond 
       [(not (andmap valid-id? (cast ids (Listof Symbol)))) (error 'parse "AAQZ invalid id, got ~e" s)]
       [(check-duplicates ids) (error 'parse "AAQZ duplicate id, got ~e" ids)]
       [else (AppC (LamC (cast ids (Listof Symbol)) (parse body)) (map parse (cast vals (Listof Sexp))))])]

     
    ;; parsing LamC ([args : (Listof Symbol)] [body : ExprC]) 

    [(list (list (? symbol? args) ...) '=> body)  
     (cond
       [(not (andmap valid-id? (cast args (Listof Symbol)))) (error 'parse "AAQZ invalid id, got ~e" args)]
       [(check-duplicates args) (error 'parse "AAQZ duplicate id, got ~e" args)]
       [else (LamC (cast args (Listof Symbol)) (parse body))])] 
    
    ;; parsing AppC ([fun : ExprC] [args : (Listof ExprC)])   

    [(list fun args ...)
     (AppC (parse fun) (map parse (cast args (Listof Sexp))))]

    #;[other (error 'parse "AAQZ expected valid syntax, got ~e" s)]))    


;; searches a given Env, returning the Value of a given variable
(define (lookup [for : Symbol] [env : Env]) : Value
  (match env
    ['() (error 'lookup "AAQZ ~e doesn't exist" for)]
    [(cons f r) (cond
                  [(equal? (Binding-name f) for) (Binding-val f)]
                  [else (lookup for r)])])) 

;; evaluates primitive operations, returning the resulting Value
(define (eval-primops [op : Symbol] [valLst : (Listof Value)]) : Value
  (match op
    ['+ (cond
          [(not (equal? (length valLst) 2))
           (error 'eval-primops "AAQZ addition requires two arguments, received ~e" valLst)]
          [else (match valLst
                  [(list (? NumV? l) (? NumV? r)) (NumV (+ (NumV-n l) (NumV-n r)))]
                  [other (error 'eval-primops "AAQZ attempted addition on non Number operands, received ~e"
                                valLst)])])]
    ['* (cond
          [(not (equal? (length valLst) 2))
           (error 'eval-primops "AAQZ multiplication requires two arguments, received ~e" valLst)]
          [else (match valLst
                  [(list (? NumV? l) (? NumV? r)) (NumV (* (NumV-n l) (NumV-n r)))]
                  [other (error 'eval-primops "AAQZ attempted multiplication on non Number operands, received ~e"
                                valLst)])])]
    ['- (cond
          [(not (equal? (length valLst) 2))
           (error 'eval-primops "AAQZ subtraction requires two arguments, received ~e" valLst)]
          [else (match valLst
                  [(list (? NumV? l) (? NumV? r)) (NumV (- (NumV-n l) (NumV-n r)))]
                  [other (error 'eval-primops "AAQZ attempted subtraction on non Number operands, received ~e"
                                valLst)])])]
    ['/ (cond
          [(not (equal? (length valLst) 2))
           (error 'eval-primops "AAQZ division requires two arguments, received ~e" valLst)]
          [else (match valLst
                  [(list (? NumV? l) (? NumV? r))
                   (cond
                     [(equal? (NumV-n r) 0) (error 'eval-primops "AAQZ divide by 0 error")]
                     [else (NumV (/ (NumV-n l) (NumV-n r)))])]
                  [other (error 'eval-primops "AAQZ attempted division on non Number operands, received ~e"
                                valLst)])])]
    ['<= (cond
           [(not (equal? (length valLst) 2))
            (error 'eval-primops "AAQZ numeric comparison requires two arguments, received ~e" valLst)]
           [else (match valLst
                   [(list (? NumV? l) (? NumV? r)) (BoolV (<= (NumV-n l) (NumV-n r)))]
                   [other (error 'eval-primops "AAQZ attempted numeric comparison on non Number operands")])])]
    ['equal? (match valLst
               [(list (? NumV? l) (? NumV? r)) (BoolV (equal? (NumV-n l) (NumV-n r)))]
               [(list (? StrV? l) (? StrV? r)) (BoolV (equal? (StrV-contents l) (StrV-contents r)))]
               [(list (? BoolV? l) (? BoolV? r)) (BoolV (equal? (BoolV-b l) (BoolV-b r)))]
               [other (BoolV #f)])]
    ['println (cond
                [(not (equal? (length valLst) 1))
                 (error 'eval-primops "AAQZ println expected one argument, received ~e" valLst)]
                [else (match valLst
                        [(list (? StrV? s)) (printf "~a\n" (StrV-contents s)) (BoolV #t)]
                        [other (error 'eval-primops "AAQZ println expected a String, received ~e" (first valLst))])])]
    ['read-num (printf "> ") (flush-output) (define num (read))
               (match num
                 [(? real? num) (NumV num)]
                 [other (error 'eval-primops "AAQZ read-num expected numerical input")])]
    ['read-str (printf "> ") (flush-output) (define str (read-line)) (match str
                                                                       [(? string? str) (StrV str)])]
    ['seq (last valLst)]
    ['++ (StrV (append-strs valLst))]))

;; helper function that takes in a list of values, coerces them to strings and joins
;; them together
(define (append-strs [lst : (Listof Value)]) : String
  (match lst
    ['() ""]
    [(cons f r) (string-append
                 (match f
                   [(? NumV? num) (number->string (NumV-n num))]
                   [(? StrV? str) (StrV-contents str)])
                 (append-strs r))]))

;; helper function that takes in a Listof Symbol and an Environment
;; returns the Environment extended with the new bindings between the params and interped args
(define (extend-multiple [names : (Listof Symbol)] [vals : (Listof Value)] [env : Env]) : Env
  (match (list names vals)
    [(list '() '()) env]
    [(list (cons f-n r-n) (cons f-v r-v)) (extend-multiple r-n r-v (extend-env (Binding f-n f-v) env))]))

;; evaluates a given ExprC in an environment and returns a Value
(define (interp [expn : ExprC] [env : Env]) : Value
  (match expn
    [(NumC n) (NumV n)]
    [(IdC name) (lookup name env)]
    [(StrC contents) (StrV contents)]
    ;; IfC case

    [(IfC test then else) (define test-res (interp test env))
                          (cond
                            [(BoolV? test-res) (cond
                                                 [(equal? test-res (BoolV #t)) (interp then env)]
                                                 [else (interp else env)])]
                            [else (error 'interp "AAQZ expected a BoolV value")])]
    [(LamC args body) (CloV args body env)]
    ;; AppC case
    ;; 1. interp func. in current env 
    ;; 2. interp args. in current env
    ;; 3. CloV case or PrimV case 
    [(AppC fun args) (define f-value (interp fun env))
                     (define args-values (map (lambda ([arg : ExprC]) (interp arg env)) args))
                     ;; interped function should either be a CloV or PrimV
                     (match f-value 
                       ;; CloV number of params. and number of args. must match
                       [(CloV params body env)
                        (define num-args (length args-values))
                        (define num-params (length params)) 
                        (cond
                          [(> num-args num-params) (error 'interp "AAQZ received too many arguments")]
                          [(< num-args num-params) (error 'interp "AAQZ received too few arguments")]
                          [else (interp body (extend-multiple params args-values env))])]
                       [(PrimV op)
                        (cond 
                          [(equal? op 'error) (define ser-val (serialize (first args-values)))
                                              (error 'interp "AAQZ user-error: ~e" ser-val)]
                          [else (eval-primops op args-values)])]
                       [other (error 'interp "AAQZ test")])]
    
    #;[other (error 'interp "AAQZ received poor input ~e" expn)]))

;; parses and evaluates an sexp in the top environment
(define (top-interp [s : Sexp]) : String 
  (serialize (interp (parse s) top-env))) 


;(top-interp example-program)  

;; Parse Testcases  

(check-equal? (parse '{(() => 9)}) (AppC (LamC '() (NumC 9)) '()))
(check-equal? (parse '{(a x y) => {(w) => (+ z (w y))}})
              (LamC '(a x y) (LamC '(w) (AppC (IdC '+) (list (IdC 'z) (AppC (IdC 'w) (list (IdC 'y))))))))
(check-equal? (parse 4) (NumC 4))
(check-equal? (parse "bob") (StrC "bob")) 
(check-equal? (parse 'x) (IdC 'x)) 
(check-equal? (parse '{(g) => 15}) (LamC '(g) (NumC 15)))  
(check-equal? (parse '{(a b c) => 3}) (LamC '(a b c) (NumC 3))) 
(check-equal? (parse '{bind [x = 4] [y = 7] {+ x y}}) 
              (AppC (LamC '(x y) (AppC (IdC '+) (list (IdC 'x) (IdC 'y)))) (list (NumC 4) (NumC 7))))
(check-equal? (parse '{bind [+ = 4] {* 2 +}})
              (AppC (LamC '(+) (AppC (IdC '*) (list (NumC 2) (IdC '+)))) (list (NumC 4))))
(check-equal? (parse '{if [equal? x 5] {+ x 5} {+ x 2}})
              (IfC (AppC (IdC 'equal?) (list (IdC 'x) (NumC 5)))
                   (AppC (IdC '+) (list (IdC 'x) (NumC 5))) (AppC (IdC '+) (list (IdC 'x) (NumC 2)))))
(check-equal? (parse '{((seven) => (seven)) (((minus) => (() => (minus (+ 3 10) (* 2 3)))) ((x y) => (+ x (* -1 y))))})
              (AppC (LamC '(seven) (AppC (IdC 'seven) '()))
                    (list (AppC
                           (LamC '(minus) (LamC '() (AppC (IdC 'minus)
                                                          (list (AppC (IdC '+)
                                                                      (list (NumC 3) (NumC 10)))
                                                                (AppC (IdC '*) (list (NumC 2) (NumC 3)))))))
                           (list (LamC '(x y) (AppC (IdC '+)
                                                    (list (IdC 'x) (AppC (IdC '*) (list (NumC -1) (IdC 'y)))))))))))

(check-exn (regexp (regexp-quote "parse: AAQZ duplicate id, got '(x x)"))   
           (lambda () (parse '{bind [x = 4] [x = 5] {+ x y}})))  
(check-exn (regexp (regexp-quote "parse: AAQZ duplicate id, got '(x x)"))   
           (lambda () (parse '{parse '((x x) => 3)})))  
(check-exn (regexp (regexp-quote "parse: AAQZ invalid id, got '(if b c)"))   
           (lambda () (parse '{(if b c) => 3})))  
(check-exn (regexp (regexp-quote "parse: AAQZ received an invalid IdC, got 'if"))
           (lambda () (parse '{(if) 20}))) 
(check-exn (regexp (regexp-quote "parse: AAQZ invalid id, got '(bind (if = 2) (bind (y = (+ 1 x)) (+ x y)))"))  
           (lambda () (parse '{bind [if = 2] {bind [y = {+ 1 x}] { + x y }}})))
(check-exn (regexp (regexp-quote "parse: AAQZ received an invalid IdC, got 'if"))  
           (lambda () (parse 'if))) 

;; Top-interp Testcases

(check-equal? (top-interp '{bind [x = 4] [y = 7] {+ x y}}) "11")
(check-equal? (top-interp '{bind [x = 10] [y = 5] {- x y}}) "5")
(check-equal? (top-interp '{bind [x = 28] [y = 7] {/ x y}}) "4")
(check-equal? (top-interp '{bind [x = 4] [y = 7] {* x y}}) "28")
(check-equal? (top-interp '4) "4")
(check-equal? (top-interp "turtle") "\"turtle\"")
(check-equal? (top-interp '+) "#<primop>")
(check-equal? (top-interp '-) "#<primop>")
(check-equal? (top-interp '/) "#<primop>")
(check-equal? (top-interp '*) "#<primop>")
(check-equal? (top-interp 'equal?) "#<primop>")
(check-equal? (top-interp 'error) "#<primop>")
(check-equal? (top-interp '<=) "#<primop>")
(check-equal? (top-interp 'true) "true")
(check-equal? (top-interp 'false) "false")  
(check-equal? (top-interp '{((seven) => (seven))
                            (((minus) => (() => (minus (+ 3 10) (* 2 3)))) ((x y) => (+ x (* -1 y))))}) "7")
(check-exn (regexp (regexp-quote "interp: AAQZ test"))
           (lambda () (top-interp '(3 4 5))))
(check-exn (regexp (regexp-quote "eval-primops: AAQZ println expected one argument, received "))   
           (lambda () (top-interp '{println "this" "is" "halloween"})))
#;(check-exn (regexp (regexp-quote "eval-primops: AAQZ println expected a String, received "))   
             (lambda () (top-interp '{println 3})))
#;(check-equal? (top-interp '{seq {println "(1/2) Seq test"} {println "(2/2) should return string 5"} {+ 3 2}}) "5")
(check-equal? (top-interp '{++ "ninety" 90 "Plus" 40}) "\"ninety90Plus40\"")

; Interp Testcases

(check-equal? (interp (NumC 3) mt-env) (NumV 3))
(check-equal? (interp (IdC 'x) (list (Binding 'x (NumV 9)))) (NumV 9))
(check-equal? (interp (StrC "world") mt-env) (StrV "world"))
(check-equal? (interp (IdC '+) top-env) (PrimV '+))
(check-equal? (interp (LamC '(x y z)
                            (AppC (IdC '*) (list (AppC (IdC '+) (list (IdC 'x) (IdC 'y))) (IdC 'z))))
                      mt-env)
              (CloV '(x y z)
                    (AppC (IdC '*) (list (AppC (IdC '+) (list (IdC 'x) (IdC 'y))) (IdC 'z)))
                    mt-env))

(check-equal? (interp (AppC (IdC '+) (list (NumC 2) (NumC 5))) top-env) (NumV 7))
(check-equal? (interp (AppC (LamC '(x y)
                                  (AppC (IdC '*) (list (IdC 'x) (IdC 'y))))
                            (list (NumC 4) (NumC 7))) top-env)
              (NumV 28))

(check-equal? (interp
               (AppC (LamC '(f) (AppC (AppC (IdC 'f) (list (NumC 3))) (list (NumC 7))))
                     (list (LamC '(x) (LamC '(y) (AppC (IdC '+) (list (IdC 'x) (IdC 'y))))))) top-env)
              (NumV 10))
(check-equal? (interp (IfC (AppC (LamC '(x) (AppC (IdC '<=) (list (NumC 2) (IdC 'x))))
                                 (list (IdC 'x))) 
                           (AppC (IdC '+)
                                 (list (IdC 'x) (NumC 99)))
                           (StrC "you lose"))
                      (extend-env (Binding 'x (NumV 3)) top-env))
              (NumV 102))
(check-exn (regexp (regexp-quote "interp: AAQZ received too many arguments"))
           (lambda () (interp (AppC (LamC '(x y) (AppC (IdC '+) (list (IdC 'x) (IdC 'y))))
                                    (list (NumC 3) (NumC 2) (NumC 9))) top-env)))
(check-exn (regexp (regexp-quote "interp: AAQZ received too few arguments"))
           (lambda () (interp (AppC (LamC '(x y) (AppC (IdC '+) (list (IdC 'x) (IdC 'y)))) (list (NumC 3))) top-env)))
(check-exn (regexp (regexp-quote "eval-primops: AAQZ multiplication requires two arguments, received "))
           (lambda () (interp (AppC (IdC '*) (list (NumC 2) (NumC 9) (NumC 7))) top-env)))
(check-equal? (interp (IfC (IdC 'false) (NumC 1) (NumC 2)) top-env)
              (NumV 2))
(check-exn (regexp (regexp-quote "interp: AAQZ expected a BoolV value"))
           (lambda () (interp (IfC (NumC 9) (NumC 1) (NumC 2)) top-env)))
(check-equal? (interp
               (IfC (AppC (LamC '(x) (AppC (IdC '<=) (list (NumC 2) (IdC 'x))))
                          (list (IdC 'x)))
                    (AppC (IdC '+)
                          (list (IdC 'x) (NumC 99)))
                    (StrC "you lose"))
               (extend-env (Binding 'x (NumV -9)) top-env))
              (StrV "you lose"))
(check-exn (regexp (regexp-quote "interp: AAQZ user-error: \"9\""))
           (lambda () (interp (AppC (IdC 'error) (list (NumC 9))) top-env)))

;; Extend-Multiple Testcases

(check-equal? (extend-multiple '(x y) (list (NumV 3) (NumV 9)) '())
              (list (Binding 'y (NumV 9)) (Binding 'x (NumV 3))))
(check-equal? (extend-multiple '(x y z track)
                               (list (NumV 3) (NumV 9) (StrV "zzz") (StrV "field"))
                               (list (Binding 'n (StrV "hello")) (Binding 'p (NumV 77))))
              (list (Binding 'track (StrV "field")) (Binding 'z (StrV "zzz")) (Binding 'y (NumV 9))
                    (Binding 'x (NumV 3)) (Binding 'n (StrV "hello")) (Binding 'p (NumV 77))))

;; Eval-Primop Testcases 

(check-equal? (eval-primops '+ (list (NumV 3) (NumV 7))) (NumV 10))
(check-exn (regexp (regexp-quote "eval-primops: AAQZ attempted addition on non Number operands"))
           (lambda () (eval-primops '+ (list (NumV 3) (StrV "nope")))))
(check-equal? (eval-primops '* (list (NumV 3) (NumV 7))) (NumV 21))
(check-exn (regexp (regexp-quote "eval-primops: AAQZ attempted multiplication on non Number operands"))
           (lambda () (eval-primops '* (list (NumV 3) (StrV "nope")))))
(check-equal? (eval-primops '- (list (NumV 3) (NumV 7))) (NumV -4))
(check-exn (regexp (regexp-quote "eval-primops: AAQZ attempted subtraction on non Number operands"))
           (lambda () (eval-primops '- (list (NumV 3) (StrV "nope")))))
(check-equal? (eval-primops '/ (list (NumV 3) (NumV 7))) (NumV (/ 3 7)))
(check-exn (regexp (regexp-quote "eval-primops: AAQZ divide by 0 error"))
           (lambda () (eval-primops '/ (list (NumV 3) (NumV 0)))))
(check-exn (regexp (regexp-quote "eval-primops: AAQZ attempted division on non Number operands"))
           (lambda () (eval-primops '/ (list (NumV 3) (StrV "nope")))))
(check-equal? (eval-primops '<= (list (NumV 7) (NumV 7))) (BoolV #t))
(check-exn (regexp (regexp-quote "eval-primops: AAQZ attempted numeric comparison on non Number operands"))
           (lambda () (eval-primops '<= (list (NumV 7) (StrV "nope")))))
(check-equal? (eval-primops 'equal? (list (NumV 3) (NumV 7))) (BoolV #f))
(check-equal? (eval-primops 'equal? (list (StrV "yep") (StrV "yep"))) (BoolV #t))
(check-equal? (eval-primops 'equal? (list (BoolV #f) (BoolV #f))) (BoolV #t))
(check-equal? (eval-primops 'equal? (list (NumV 3) (PrimV '+))) (BoolV #f))
(check-equal? (eval-primops 'equal? (list (CloV '(x y z) (NumC 1) mt-env) (NumV 9))) (BoolV #f))
(check-exn (regexp (regexp-quote "eval-primops: AAQZ addition requires two arguments, received "))
           (lambda () (eval-primops '+ (list (NumV 3) (NumV 1) (NumV 7)))))
(check-exn (regexp (regexp-quote "eval-primops: AAQZ multiplication requires two arguments, received "))
           (lambda () (eval-primops '* (list (NumV 3) (NumV 1) (NumV 7)))))
(check-exn (regexp (regexp-quote "eval-primops: AAQZ subtraction requires two arguments, received "))
           (lambda () (eval-primops '- (list (NumV 3) (NumV 1) (NumV 7)))))
(check-exn (regexp (regexp-quote "eval-primops: AAQZ division requires two arguments, received "))
           (lambda () (eval-primops '/ (list (NumV 3) (NumV 1) (NumV 7)))))
(check-exn (regexp (regexp-quote "eval-primops: AAQZ numeric comparison requires two arguments, received "))
           (lambda () (eval-primops '<= (list (NumV 3) (NumV 1) (StrV "no")))))
#;(check-equal? (eval-primops 'println (list (StrV "3"))) (BoolV #t))
(check-equal? (eval-primops '++ (list (NumV 3) (StrV "hello") (NumV 67) (StrV "_ howdy"))) (StrV "3hello67_ howdy"))

;; Lookup Testcases

(check-exn (regexp (regexp-quote "lookup: AAQZ 'x doesn't exist"))
           (lambda () (lookup 'x '())))
(check-equal? (lookup 'x (list (Binding 'g (NumV 3)) (Binding 'x (NumV 9)))) (NumV 9))

;; Serialize Testcases

(check-equal? (serialize (NumV 34)) "34")
(check-equal? (serialize (StrV "big dog")) "\"big dog\"")
(check-equal? (serialize (BoolV #t)) "true")
(check-equal? (serialize (BoolV #f)) "false")
(check-equal? (serialize (CloV '(x y z) (NumC 9) mt-env)) "#<procedure>")
(check-equal? (serialize (PrimV '-)) "#<primop>")