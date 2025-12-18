# Ejercicios car cdr

## 1

```lisp
(setq lista '(a b c d e f g))
(car (cdddr lista))
```

## 2

```lisp
(setq lista '(1 2 3 4 5 6))
(cdddr lista)
```

## 3

```lisp
(setq lista '(a b c d e))
(setq nlista
      (cons (car lista)
            (cons (car (cdr lista))
                  (cons (car (cdr (cdr lista))) nil))))
```

## 4

```lisp
(setq lista '(x y z w))
(setq nlista
      (cons (cadr lista)
            (cons (caddr lista) nil)))
```

## 5

```lisp
(setq lista '(10 20 30 40))
(setq suma
      (cons (caddr lista)
            (cons (cadr lista)
                  (cons (car lista) nil))))
```
