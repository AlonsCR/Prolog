; Problema 1.1
(car (cdr (cdr (cdr '(a b c d e)))))

; Problema 1.2
(car (car (cdr (cdr '((1 2) (3 4) (5 6))))))

; Problema 1.3
(car (car (cdr (cdr '((a b) (c d) (e f))))))

; Problema 1.4
(car (car (cdr (cdr '((x y) ((p q) (r s)) (z w))))))

; Problema 1.5
(car (cdr (car (cdr (car (cdr '((1 (2 3)) (4 (5 6)))))))))

; Problema 1.6
(car (cdr (car '(((a b) c) d e))))

; Problema 1.7
(car (cdr (car (cdr '(((1 2) 3) ((4 5) 6))))))

; Problema 1.8
(car (cdr (car (cdr (car '((p (q (r s))) t u))))))

; Problema 1.9
(car (car (cdr (car (cdr '(((a) b) (c (d e)) f))))))

; Problema 1.10
(car (car (cdr (car (cdr (car '((1 (2 (3 4))) (5 6))))))))

; Problema 1.11
(car (cdr (car (cdr '(((x) (y)) ((z) (w)))))))

; Problema 1.12
(car (car (cdr (car (cdr (car '((a (b (c d))) (e f))))))))

; Problema 1.13
(car (car (cdr (car (cdr (car (cdr (car '((1 (2 (3 (4 5)))) (6 7))))))))))

; Problema 1.14
(car (car (car (cdr (cdr '(((a b) c) ((d e) f) ((g h) i)))))))

; Problema 1.15
(car (car (cdr (car (cdr '(((x y) (z w)) ((p q) (r s))))))))

; Problema 1.16
(car (car (cdr (car (cdr (car (cdr (car (cdr (car '((1 (2 (3 (4 (5 6))))) (7 8))))))))))))

; Problema 1.17
(car (car (cdr (car (cdr (car (cdr (car '((a (b (c (d e)))) (f g))))))))))

; Problema 1.18
(car (car (cdr (car (cdr '(((1 2) (3 4)) ((5 6) (7 8))))))))

; Problema 1.19
(car (car (cdr (car (cdr (car (cdr (car '((x (y (z (w v)))))))))))))

; Problema 1.20
(car (car (cdr (car (cdr '(((a b c) (d e f)) ((g h i) (j k l))))))))

; Ejercicio 2.1
(setq persona '((nombre . "Ana") (edad . 23) (ciudad . "Morelia")))
(cdr (assoc 'edad persona))

; Ejercicio 2.2
(defun mi-funcion (lista)
  (if (> (car lista) 0)
      "positivo"
      "no positivo"))

; Ejercicio 2.3
(defun dobles-pares (lista)
  (remove nil (mapcar #'(lambda (x) (if (evenp x) (* x 2) nil)) lista)))

; Ejercicio 2.4
(defun tipo-color (color)
  (cond
    ((eq color 'rojo) "Color cálido")
    ((eq color 'azul) "Color frío")
    ((eq color 'verde) "Color neutro")
    (t "Color desconocido")))

; Ejercicio 2.5
(defun clasificar-dia (dia)
  (case dia
    ((lunes martes miercoles jueves viernes) "día laboral")
    ((sabado domingo) "fin de semana")))

; Ejercicio 2.6
(defun imprimir-primero (lista)
  (when lista
    (print (car lista))))

; Ejercicio 2.7
(defun verificar-vacia (lista)
  (unless lista
    (print "lista vacía")))

; Ejercicio 2.8
(defun primeros-elementos (lista)
  (mapcar #'car lista))

; Ejercicio 2.9
(setq alumnos '((juan . 8) (maria . 10) (ana . 9)))
(defun estado-alumno (nombre)
  (if (>= (cdr (assoc nombre alumnos)) 8)
      "Aprobado"
      "Reprobado"))

; Ejercicio 2.10
(defun evaluar-lista (lista)
  (cond
    ((null lista) "vacía")
    ((null (cdr lista)) "un solo elemento")
    (t "larga")))