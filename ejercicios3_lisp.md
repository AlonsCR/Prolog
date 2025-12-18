; Ejercicio 1
(defun n-esimo (n lista)
  (if (= n 1)
      (car lista)
      (n-esimo (- n 1) (cdr lista))))

; Ejercicio 2
(defun filtra-positivos (lista)
  (let ((resultado nil))
    (dolist (x lista)
      (when (> x 0)
        (setq resultado (append resultado (list x)))))
    resultado))

; Ejercicio 3
(defun clasifica-numero (n)
  (cond
    ((< n 0) "Negativo")
    ((= n 0) "Cero")
    ((<= n 10) "Pequeño")
    ((<= n 100) "Mediano")
    (t "Grande")))

; Ejercicio 4
(defun suma-pares (lista)
  (let ((suma 0))
    (dolist (x lista)
      (unless (oddp x)
        (setq suma (+ suma x))))
    suma))

; Ejercicio 5
(defun procesa-lista (lista)
  (cond
    ((null lista) "Lista vacía")
    ((and (numberp (car lista)) (> (car lista) 50)) "Grande")
    ((listp (car lista)) "Sublista detectada")
    (t "Caso general")))

; Ejemplos de ejecución
; Ejercicio 1
(n-esimo 3 '(a b c d e))

; Ejercicio 2
(filtra-positivos '(-2 0 3 -5 7))

; Ejercicio 3
(clasifica-numero 57)
(clasifica-numero -5)
(clasifica-numero 0)
(clasifica-numero 7)
(clasifica-numero 150)

; Ejercicio 4
(suma-pares '(1 2 3 4 5 6))

; Ejercicio 5
(procesa-lista '())
(procesa-lista '(60 1 2))
(procesa-lista '((1 2) 3 4))
(procesa-lista '(10 20 30))