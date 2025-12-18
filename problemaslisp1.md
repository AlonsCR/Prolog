(defun calcular-sueldo (anios)
  (let ((sueldo-base 40000))
    (cond
      ((> anios 10) (* sueldo-base 1.10))
      ((> anios 5) (* sueldo-base 1.07))
      ((> anios 3) (* sueldo-base 1.05))
      (t (* sueldo-base 1.03)))))

(defun lavadora (peso)
  (cond
    ((> peso 30) (format t "Error: Demasiado peso. La lavadora no funcionara.~%"))
    ((>= peso 22) (format t "Nivel: Maximo~%Litros de agua: 60~%"))
    ((>= peso 15) (format t "Nivel: Alto~%Litros de agua: 45~%"))
    ((>= peso 8) (format t "Nivel: Medio~%Litros de agua: 30~%"))
    (t (format t "Nivel: Minimo~%Litros de agua: 15~%"))))

(defun fiesta-quince (edad)
  (cond
    ((> edad 15) (format t "Puede entrar solo si trae regalo.~%"))
    ((= edad 15) (format t "Entra totalmente gratis.~%"))
    (t (format t "No puede entrar a la fiesta.~%"))))

;; Ejemplos de ejecución
(calcular-sueldo 12)
(lavadora 25)
(fiesta-quince 16)