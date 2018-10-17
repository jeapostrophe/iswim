#lang iswim 

;; Numbers
zero := (λ (s z) z)
succ := (λ (n)
          (λ (s z) (s (n s z))))
one := (succ zero)
two := (succ one)
three := (succ two)

plus := (λ (x y)
          (λ (s z)
            (x s (y s z))))

mult := (λ (x y)
          (λ (s z)
            (x (y s) z)))

;; Booleans
true := (λ (t f) t)
false := (λ (t f) f)
if := (λ (c t f) (c t f))

zero? := (λ (n) (n (λ (x) false) true))

;; Pairs
pair := (λ (l r) (λ (c) (if c l r)))
fst := (λ (p) (p true))
snd := (λ (p) (p false))

pred := (λ (n) (fst (n (λ (x) (pair (snd x) (succ (snd x)))) (pair zero zero))))

;; Recursion
Z := (λ (f) ((λ (x) (f (λ (v) (x x v))))
             (λ (x) (f (λ (v) (x x v))))))

;; Factorial
mkfac := (λ (fac)
           (λ (n)
             ((if (zero? n)
                (λ (x) one)
                (λ (x) (mult n (fac (pred n)))))
              zero)))
fac := (Z mkfac)

to-num := (λ (n) (n add1 0))

(to-num
 (let ([x (plus (pred two) two)])
   (if (zero? one)
     three
     (fac (succ x)))))
