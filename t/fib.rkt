#lang iswim 

one := 1

times-4 := (λ x . (* x 4))

(+ one (times-4 (add1 3)))
