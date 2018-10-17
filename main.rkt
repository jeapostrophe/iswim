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
(define-syntax (iswim-let stx)
  (raise-syntax-error 'iswim-let "Illegal outside #%module-begin"))

(begin-for-syntax
  (define-splicing-syntax-class idef
    #:description "ISWIM definition"
    #:attributes (x ast)
    (pattern (~seq x:id (~literal iswim-def) e:iexp)
             #:attr ast #'e.ast))
  (define-syntax-class iprim
    #:description "ISWIM primitive"
    (pattern (~literal add1))
    (pattern (~literal +))
    (pattern (~literal *)))
  (define-syntax-class iexp
    #:description "ISWIM expression"
    #:attributes (ast)
    ;; Primitives
    (pattern (o:iprim x:iexp ...)
             #:attr ast #'(Prim 'o (list x.ast ...)))
    (pattern o:iprim
             #:attr ast #'(Lam 'x (Prim 'o (list (Var 'x)))))
    ;; Constants
    (pattern n:nat
             #:attr ast #'(NumCon n))
    (pattern s:str
             #:attr ast #'(StrCon s))
    ;; Variable
    (pattern ast:id)
    ;; Let
    (pattern ((~literal iswim-let) ([x:id xm:iexp] ...) bm:iexp)
             #:attr ast #'(App* (Lam* '(x ...) (let ([x (Var 'x)] ...) bm.ast))
                                (list xm.ast ...)))
    ;; Lambda
    (pattern ((~literal iswim-lam) (x:id ...+) m:iexp)
             #:attr ast #'(Lam* '(x ...) (let ([x (Var 'x)] ...) m.ast)))
    ;; App
    (pattern (m:iexp n:iexp ...+)
             #:attr ast #'(App* m.ast (list n.ast ...)))))

(define-syntax (iswim-mb stx)
  (syntax-parse stx
    [(_ (~optional
         (~or (~and (~seq #:parens)
                    (~bind [go! #'iswim-display-parens]))
              (~and (~seq #:new)
                    (~bind [go! #'iswim-display-new]))
              (~and (~seq #:fun)
                    (~bind [go! #'iswim-display-fun]))
              (~and (~seq #:tree)
                    (~bind [go! #'iswim-display-tree])))
         #:defaults ([go! #'iswim-eval]))
        d:idef ... e:iexp)
     (syntax/loc stx
       (#%module-begin
        (module+ main
          (go!
           (let* ([d.x d.ast] ...)
             e.ast)))))]))

(provide
 #%datum #%top
 add1 + *
 (rename-out
  [iswim-mb #%module-begin]
  [iswim-lam λ]
  [iswim-lam lambda]
  [iswim-def :=]
  [iswim-let let]))

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

(define (App* m ns)
  (for/fold ([f m]) ([n (in-list ns)])
    (App f n)))
(define (Lam* xs m)
  (for/fold ([m m]) ([x (in-list (reverse xs))])
    (Lam x m)))

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

(define (iswim-display-tree a)
  (local-require pict
                 pict/tree-layout
                 racket/draw
                 racket/class)
  (define @-pict (text "@"))
  (define λ-pict (text "λ"))
  (define ->tree
    (match-lambda
      [(or (Var x) (NumCon x) (StrCon x)) (tree-layout #:pict (text (~a x)))]
      [(App m n) (tree-layout #:pict @-pict (->tree m) (->tree n))]
      [(Lam x m) (tree-layout #:pict λ-pict (->tree (Var x)) (->tree m))]
      [(Prim o ms) (apply tree-layout #:pict (text (~a o))
                          (map ->tree ms))]))
  (define tl (->tree a))
  (define p (naive-layered tl))
  (define bm (pict->bitmap p
                           #:make-bitmap (λ (w h)
                                           (make-bitmap w h #f))))
  (define pth (build-path (current-directory) "iswim.png"))
  (send bm save-file pth 'png)
  (eprintf "Wrote to ~a\n" pth)
  (void))

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
