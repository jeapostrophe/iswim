#lang racket/base
(require (for-syntax racket/base
                     syntax/parse))

;; Hook into language
(module reader syntax/module-reader
  #:language 'iswim/main)

;; Core syntax
(define-syntax (iswim-lam stx)
  (raise-syntax-error 'iswim-lam "Illegal outside #%module-begin"))
(define-syntax (iswim-def stx)
  (raise-syntax-error 'iswim-def "Illegal outside #%module-begin"))

(begin-for-syntax
  (define-splicing-syntax-class idef
    #:attributes (x ast)
    (pattern (~seq x:id (~literal iswim-def) e:iexp)
             #:attr ast #'e.ast))
  (define-syntax-class iexp
    #:attributes (ast)
    ;; Unary operators
    (pattern ((~literal add1) x:iexp)
             #:attr ast #'(Prim 'add1 (list x.ast)))
    ;; Binary operators
    (pattern ((~literal +) x:iexp y:iexp)
             #:attr ast #'(Prim '+ (list x.ast y.ast)))
    (pattern ((~literal *) x:iexp y:iexp)
             #:attr ast #'(Prim '* (list x.ast y.ast)))
    ;; Constants
    (pattern n:nat
             #:attr ast #'(NumCon n))
    (pattern s:str
             #:attr ast #'(StrCon s))
    ;; Variable
    (pattern ast:id)
    ;; App
    (pattern (m:iexp n:iexp)
             #:attr ast #'(App m.ast n.ast))
    ;; Lambda
    (pattern ((~literal iswim-lam) x:id . m:iexp)
             #:attr ast #'(Lam 'x (let ([x (Var 'x)]) m.ast)))
    ))

(define-syntax (iswim-mb stx)
  (syntax-parse stx
    [(_ (~optional
         (~or (~and (~seq #:parens)
                    (~bind [go! #'iswim-display-parens]))
              (~and (~seq #:new)
                    (~bind [go! #'iswim-display-new]))
              (~and (~seq #:fun)
                    (~bind [go! #'iswim-display-fun])))
         #:defaults ([go! #'iswim-eval]))
        d:idef ... e:iexp)
     (syntax/loc stx
       (#%module-begin
        (module+ main
          (go!
           (let ([d.x d.ast] ...)
             e.ast)))))]))

(provide
 #%datum #%top
 add1 + *
 (rename-out
  [iswim-mb #%module-begin]
  [iswim-lam λ]
  [iswim-lam lambda]
  [iswim-def :=]))

;; Runtime
(require racket/match
         racket/format
         racket/pretty)

(struct Var (x) #:transparent)
(struct App (m n) #:transparent)
(struct Lam (x m) #:transparent)
(struct Prim (o ms) #:transparent)
(struct NumCon (n) #:transparent)
(struct StrCon (s) #:transparent)

(define ->sexp
  (match-lambda
    [(Var x) `(Var ',x)]
    [(App m n) `(App ,(->sexp m) ,(->sexp n))]
    [(Lam x m) `(Lam ',x ,(->sexp m))]
    [(Prim o ms) `(Prim ',o (list ,@(map ->sexp ms)))]
    [(NumCon n) `(NumCon ,n)]
    [(StrCon s) `(StrCon ,s)]))
(define (iswim-display-parens a)
  (pretty-write (->sexp a)))

(define ->new
  (match-lambda
    [(Var x) (~a "new Var(" (~v (symbol->string x)) ")")]
    [(App m n) (~a "new App(" (->new m) ", " (->new n) ")")]
    [(Lam x m) (~a "new Lam(" (~v (symbol->string x)) ", " (->new m) ")")]
    [(Prim o ms) (~a "new Prim(" (~v (symbol->string o)) ", " (->new ms) ")")]
    [(NumCon n) (~a "new NumCon(" n ")")]
    [(StrCon s) (~a "new StrCon(" (~v s) ")")]
    ['() "new ENull()"]
    [(cons a d) (~a "new ECons(" (->new a) ", " (->new d) ")")]))
(define (iswim-display-new a)
  (displayln (->new a)))
(define (iswim-display-fun a)
  (displayln (regexp-replace* #rx"new " (->new a) "")))

(define (δ o ms)
  (match* (o ms)
    [('add1 (list n)) (add1 n)]
    [('+ (list x y)) (+ x y)]
    [('* (list x y)) (* x y)]))

(define (iswim-eval* env a)
  (define (rec a) (iswim-eval* env a))
  (match a
    [(Var x) (hash-ref env x (~a "Unbound variable: " x))]
    [(App m n) ((rec m) (rec n))]
    [(Lam x m) (λ (v) (iswim-eval* (hash-set env x v) m))]
    [(Prim o ms) (δ o (map rec ms))]
    [(NumCon n) n]
    [(StrCon s) s]))

(define (iswim-eval a)
  (iswim-eval* (hasheq) a))
