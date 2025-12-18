;;; Variables dinámicas
(defvar *sintomas-reportados* '())
(defvar *modo-actual* nil)

;;; Base de datos star wars

;;; Hechos de especie
(defparameter *especies*
  '((anakin . humano)
    (yoda . desconocida)
    (chewbacca . wookiee)
    (r2d2 . droide)
    (c3po . droide)
    (darth_maul . zabrak)
    (ahsoka . togruta)
    (boba_fett . humano)
    (obi_wan . humano)
    (sidious . humano)
    (mace_windu . humano)
    (han_solo . humano)))

;;; Hechos de afiliación
(defparameter *afiliaciones*
  '((anakin . sith)
    (yoda . jedi)
    (chewbacca . rebelion)
    (r2d2 . rebelion)
    (c3po . rebelion)
    (darth_maul . sith)
    (ahsoka . jedi)
    (boba_fett . cazarrecompensas)
    (obi_wan . jedi)
    (sidious . sith)
    (mace_windu . jedi)
    (han_solo . rebelion)))

;;; Hechos de armas
(defparameter *armas*
  '((anakin . sable_de_luz)
    (yoda . sable_de_luz)
    (chewbacca . ballesta_laser)
    (r2d2 . herramientas)
    (c3po . protocolo)
    (darth_maul . sable_doble)
    (ahsoka . sables_duales)
    (boba_fett . rifle_blaster)
    (obi_wan . sable_de_luz)
    (sidious . rayos_fuerza)
    (mace_windu . sable_de_luz)
    (han_solo . blaster_dl44)))

;;; Sensibles a la fuerza
(defparameter *sensibles-fuerza*
  '(anakin yoda darth_maul ahsoka obi_wan sidious mace_windu))

;;; Afiliaciones opuestas
(defparameter *afiliaciones-opuestas*
  '((jedi . sith)
    (sith . jedi)
    (rebelion . sith)
    (sith . rebelion)))

;;; base de datos enfermedades

(defparameter *sintomas-enfermedades*
  '((epilepsia convulsiones perdida_conciencia confusion_temporal 
               aura_visual mirada_fija rigidez_muscular mordedura_lengua)
    (gota dolor_articular_agudo hinchazon_dedo_gordo enrojecimiento_piel
          calor_zona_afectada sensibilidad_tacto nodulos_piel movilidad_limitada)
    (autoinmune fatiga_cronica dolor_muscular_general erupcion_cutanea
                fiebre_recurrente perdida_cabello inflamacion_ganglios entumecimiento_manos)))

(defparameter *tratamientos*
  '((epilepsia . "Antiepilepticos, dieta cetogenica, evitar luces estroboscopicas.")
    (gota . "Colchicina, corticoides, hidratacion, dieta baja en purinas.")
    (autoinmune . "Inmunosupresores, terapia biologica, antiinflamatorios.")))

;;; fucnioes auxiliares

(defun limpiar-todo ()
  "Limpia todas las variables dinámicas"
  (setf *sintomas-reportados* '())
  (setf *modo-actual* nil))

(defun leer-entrada ()
  "Lee una línea y la convierte en lista de símbolos"
  (format t "> ")
  (force-output)
  (let ((linea (read-line *standard-input* nil "")))
    (mapcar #'(lambda (s) (intern (string-upcase s)))
            (split-string linea))))

(defun split-string (string)
  "Divide un string por espacios"
  (let ((result '())
        (current ""))
    (loop for char across string
          do (if (char= char #\Space)
                 (when (> (length current) 0)
                   (push current result)
                   (setf current ""))
                 (setf current (concatenate 'string current (string char)))))
    (when (> (length current) 0)
      (push current result))
    (nreverse result)))

;;; consultas star wars

(defun especie (personaje)
  "Obtiene la especie de un personaje"
  (cdr (assoc personaje *especies*)))

(defun afiliacion (personaje)
  "Obtiene la afiliación de un personaje"
  (cdr (assoc personaje *afiliaciones*)))

(defun arma-principal (personaje)
  "Obtiene el arma principal de un personaje"
  (cdr (assoc personaje *armas*)))

(defun es-sensible-fuerza-p (personaje)
  "Verifica si es sensible a la fuerza"
  (member personaje *sensibles-fuerza*))

(defun usa-sable-p (personaje)
  "Verifica si usa sable de luz"
  (let ((arma (arma-principal personaje)))
    (member arma '(sable_de_luz sable_doble sables_duales))))

(defun guerrero-de-la-fuerza-p (personaje)
  "Un guerrero de la fuerza es sensible y usa sable"
  (and (es-sensible-fuerza-p personaje)
       (usa-sable-p personaje)))

(defun son-enemigos-p (x y)
  "Dos personajes son enemigos si tienen afiliaciones opuestas"
  (and (not (eq x y))
       (let ((afil-x (afiliacion x))
             (afil-y (afiliacion y)))
         (or (eq (cdr (assoc afil-x *afiliaciones-opuestas*)) afil-y)
             (eq (cdr (assoc afil-y *afiliaciones-opuestas*)) afil-x)))))

(defun son-companeros-p (x y)
  "Dos personajes son compañeros si tienen la misma afiliación"
  (and (not (eq x y))
       (eq (afiliacion x) (afiliacion y))))

(defun obtener-personajes ()
  "Obtiene lista de todos los personajes"
  (mapcar #'car *especies*))

(defun obtener-guerreros-fuerza ()
  "Obtiene lista de guerreros de la fuerza"
  (remove-if-not #'guerrero-de-la-fuerza-p (obtener-personajes)))

(defun obtener-enemigos (personaje)
  "Obtiene enemigos de un personaje"
  (remove-if-not #'(lambda (p) (son-enemigos-p personaje p))
                 (obtener-personajes)))

(defun obtener-companeros (personaje)
  "Obtiene compañeros de un personaje"
  (remove-if-not #'(lambda (p) (son-companeros-p personaje p))
                 (obtener-personajes)))

(defun obtener-por-afiliacion (afil)
  "Obtiene personajes por afiliación"
  (mapcar #'car 
          (remove-if-not #'(lambda (x) (eq (cdr x) afil)) *afiliaciones*)))

;;; mostrar star wars

(defun mostrar-personajes ()
  "Muestra todos los personajes"
  (format t "~%Personajes star wars~%")
  (dolist (p (obtener-personajes))
    (format t "  - ~A (~A, ~A)~%" p (especie p) (afiliacion p))))

(defun mostrar-info-personaje (nombre)
  "Muestra información completa de un personaje"
  (if (especie nombre)
      (progn
        (format t "~%-------~%")
        (format t "  Personaje: ~A~%" nombre)
        (format t "------~%")
        (format t "  Especie: ~A~%" (especie nombre))
        (format t "  Bando: ~A~%" (afiliacion nombre))
        (format t "  Arma principal: ~A~%" (arma-principal nombre))
        (format t "  Sensible a la Fuerza: ~A~%" 
                (if (es-sensible-fuerza-p nombre) "SI" "NO"))
        (format t "  Guerrero de la Fuerza: ~A~%" 
                (if (guerrero-de-la-fuerza-p nombre) "SI" "NO"))
        (format t "----~%"))
      (format t "No conozco a: ~A~%Escribe 'personajes' para ver la lista.~%" nombre)))

(defun consultar-especie (nombre)
  (let ((esp (especie nombre)))
    (if esp
        (format t "~A es de especie: ~A~%" nombre esp)
        (format t "No conozco a: ~A~%" nombre))))

(defun consultar-afiliacion (nombre)
  (let ((afil (afiliacion nombre)))
    (if afil
        (format t "~A pertenece a: ~A~%" nombre afil)
        (format t "No conozco a: ~A~%" nombre))))

(defun consultar-arma (nombre)
  (let ((arma (arma-principal nombre)))
    (if arma
        (format t "~A usa: ~A~%" nombre arma)
        (format t "No conozco a: ~A~%" nombre))))

(defun mostrar-guerreros-fuerza ()
  (format t "~%Guerreros de la fuerza~%")
  (format t "(Sensibles a la Fuerza que usan sable de luz)~%~%")
  (let ((guerreros (obtener-guerreros-fuerza)))
    (if guerreros
        (dolist (g guerreros)
          (format t "  - ~A [~A] - ~A~%" g (afiliacion g) (arma-principal g)))
        (format t "No hay guerreros de la fuerza registrados.~%")))
  (format t "---~%"))

(defun verificar-guerrero-fuerza (nombre)
  (cond
    ((guerrero-de-la-fuerza-p nombre)
     (format t "Si, ~A es un Guerrero de la Fuerza.~%" nombre)
     (format t "  (Es sensible a la Fuerza y usa sable de luz)~%"))
    ((especie nombre)
     (format t "NO, ~A NO es un Guerrero de la Fuerza.~%" nombre)
     (if (es-sensible-fuerza-p nombre)
         (format t "  (Es sensible a la Fuerza pero no usa sable)~%")
         (format t "  (No es sensible a la Fuerza)~%")))
    (t (format t "No conozco a: ~A~%" nombre))))

(defun mostrar-enemigos (nombre)
  (if (especie nombre)
      (let ((enemigos (obtener-enemigos nombre)))
        (format t "~%Enemigo de ~A (~A) -~%" nombre (afiliacion nombre))
        (if enemigos
            (dolist (e enemigos)
              (format t "  - ~A [~A]~%" e (afiliacion e)))
            (format t "  No tiene enemigos registrados.~%")))
      (format t "No conozco a: ~A~%" nombre)))

(defun mostrar-companeros (nombre)
  (if (especie nombre)
      (let ((companeros (obtener-companeros nombre)))
        (format t "~%Comapañero de ~A (~A) ===~%" nombre (afiliacion nombre))
        (if companeros
            (dolist (c companeros)
              (format t "  - ~A~%" c))
            (format t "  No tiene companeros registrados.~%")))
      (format t "No conozco a: ~A~%" nombre)))

(defun mostrar-por-afiliacion (afil)
  (let ((personajes (obtener-por-afiliacion afil)))
    (if personajes
        (progn
          (format t "~%=== Miembros de ~A ===~%" afil)
          (dolist (p personajes)
            (format t "  - ~A~%" p)))
        (format t "No hay personajes en: ~A~%" afil))))

(defun mostrar-sensibles-fuerza ()
  (format t "~%Sensibles a la fuerza~%")
  (dolist (s *sensibles-fuerza*)
    (format t "  - ~A [~A]~%" s (afiliacion s)))
  (format t "-----~%"))

(defun verificar-enemigos (x y)
  (cond
    ((son-enemigos-p x y)
     (format t "SI, ~A y ~A son ENEMIGOS.~%" x y)
     (format t "  ~A es ~A~%" x (afiliacion x))
     (format t "  ~A es ~A~%" y (afiliacion y)))
    ((and (especie x) (especie y))
     (format t "NO, ~A y ~A no son enemigos.~%" x y))
    (t (format t "No conozco a uno o ambos personajes.~%"))))

(defun verificar-companeros (x y)
  (cond
    ((son-companeros-p x y)
     (format t "SI, ~A y ~A son COMPANEROS.~%" x y)
     (format t "  Ambos pertenecen a: ~A~%" (afiliacion x)))
    ((and (especie x) (especie y))
     (format t "NO, ~A y ~A no son companeros.~%" x y)
     (format t "  ~A es ~A~%" x (afiliacion x))
     (format t "  ~A es ~A~%" y (afiliacion y)))
    (t (format t "No conozco a uno o ambos personajes.~%"))))

(defun verificar-sensible-fuerza (nombre)
  (cond
    ((es-sensible-fuerza-p nombre)
     (format t "SI, ~A es sensible a la Fuerza.~%" nombre))
    ((especie nombre)
     (format t "NO, ~A NO es sensible a la Fuerza.~%" nombre))
    (t (format t "No conozco a: ~A~%" nombre))))

;;; diagnostico de enfermedades

(defun obtener-sintomas-enfermedad (enfermedad)
  "Obtiene los síntomas de una enfermedad"
  (cdr (assoc enfermedad *sintomas-enfermedades*)))

(defun es-sintoma-conocido-p (sintoma)
  "Verifica si el síntoma está en alguna enfermedad"
  (some #'(lambda (e) (member sintoma (cdr e)))
        *sintomas-enfermedades*))

(defun obtener-enfermedades-por-sintoma (sintoma)
  "Obtiene las enfermedades que tienen ese síntoma"
  (mapcar #'car
          (remove-if-not #'(lambda (e) (member sintoma (cdr e)))
                         *sintomas-enfermedades*)))

(defun agregar-sintoma-reportado (sintoma)
  "Agrega un síntoma a la lista de reportados"
  (cond
    ((not (es-sintoma-conocido-p sintoma))
     (format t "No reconozco: ~A~%" sintoma))
    ((member sintoma *sintomas-reportados*)
     (format t "Ya reportaste: ~A~%" sintoma))
    (t
     (push sintoma *sintomas-reportados*)
     (format t "Registrado: ~A~%" sintoma)
     (format t "Asociado a: ~A~%" (obtener-enfermedades-por-sintoma sintoma))
     (format t "Otro sintoma? o 'listo' para diagnostico.~%"))))

(defun mostrar-sintomas-reportados ()
  (if (null *sintomas-reportados*)
      (format t "Sin sintomas.~%")
      (progn
        (format t "Tus sintomas:~%")
        (dolist (s *sintomas-reportados*)
          (format t "  - ~A~%" s)))))

(defun limpiar-sintomas-reportados ()
  (setf *sintomas-reportados* '()))

(defun contar-coincidencias (enfermedad sintomas-reportados)
  "Cuenta cuántos síntomas coinciden"
  (let ((sintomas-enf (obtener-sintomas-enfermedad enfermedad)))
    (length (intersection sintomas-reportados sintomas-enf))))

(defun realizar-diagnostico-auto ()
  "Realiza el diagnóstico automático"
  (if (null *sintomas-reportados*)
      (format t "No reportaste sintomas.~%")
      (progn
        (format t "~%Diagnostico~%")
        (format t "Sintomas reportados:~%")
        (dolist (s *sintomas-reportados*)
          (format t "  * ~A~%" s))
        (format t "~%Analisis:~%")
        (let ((resultados '()))
          (dolist (enf '(epilepsia gota autoinmune))
            (let* ((sintomas-enf (obtener-sintomas-enfermedad enf))
                   (coincidencias (contar-coincidencias enf *sintomas-reportados*))
                   (total (length sintomas-enf)))
              (format t "  ~A: ~A/~A~A~%" 
                      enf coincidencias total
                      (if (>= coincidencias 3) " (posible)" ""))
              (push (cons enf coincidencias) resultados)))
          ;; Determinar diagnóstico final
          (let* ((mejor (reduce #'(lambda (a b) (if (> (cdr a) (cdr b)) a b)) resultados))
                 (enfermedad (car mejor))
                 (max-count (cdr mejor)))
            (format t "~%")
            (if (>= max-count 3)
                (progn
                  (format t "Diagnostico probable: ~A~%" enfermedad)
                  (format t "Tratamiento: ~A~%" 
                          (cdr (assoc enfermedad *tratamientos*))))
                (format t "Sintomas insuficientes. Consulte a un medico.~%")))))))

(defun mostrar-sintomas-enfermedad (enfermedad)
  (let ((sintomas (obtener-sintomas-enfermedad enfermedad)))
    (if sintomas
        (progn
          (format t "Sintomas de ~A:~%" enfermedad)
          (dolist (s sintomas)
            (format t "  - ~A~%" s)))
        (format t "No conozco: ~A~%" enfermedad))))

(defun mostrar-tratamiento (enfermedad)
  (let ((trat (cdr (assoc enfermedad *tratamientos*))))
    (if trat
        (format t "Tratamiento: ~A~%" trat)
        (format t "No conozco: ~A~%" enfermedad))))

;;; back-end star

(defun procesar-starwars (input)
  "Procesa comandos del módulo Star Wars"
  (cond
    ;; Lista de personajes
    ((or (equal input '(PERSONAJES))
         (equal input '(LISTA PERSONAJES))
         (equal input '(QUIENES SON LOS PERSONAJES)))
     (mostrar-personajes))
    
    ;; Info de personaje
    ((and (>= (length input) 3)
          (member (first input) '(INFO INFORMACION))
          (eq (second input) 'DE))
     (mostrar-info-personaje (third input)))
    
    ((and (>= (length input) 3)
          (eq (first input) 'QUIEN)
          (eq (second input) 'ES))
     (mostrar-info-personaje (third input)))
    
    ((and (>= (length input) 3)
          (eq (first input) 'DIME)
          (eq (second input) 'SOBRE))
     (mostrar-info-personaje (third input)))
    
    ;; Especie
    ((and (>= (length input) 3)
          (eq (first input) 'ESPECIE)
          (eq (second input) 'DE))
     (consultar-especie (third input)))
    
    ((and (>= (length input) 4)
          (equal (subseq input 0 3) '(QUE ESPECIE ES)))
     (consultar-especie (fourth input)))
    
    ;; Afiliación
    ((and (>= (length input) 3)
          (eq (first input) 'AFILIACION)
          (eq (second input) 'DE))
     (consultar-afiliacion (third input)))
    
    ((and (>= (length input) 4)
          (equal (subseq input 0 3) '(A QUE PERTENECE)))
     (consultar-afiliacion (fourth input)))
    
    ;; Arma
    ((and (>= (length input) 3)
          (eq (first input) 'ARMA)
          (eq (second input) 'DE))
     (consultar-arma (third input)))
    
    ((and (>= (length input) 4)
          (equal (subseq input 0 3) '(QUE ARMA USA)))
     (consultar-arma (fourth input)))
    
    ;; Guerreros de la fuerza
    ((or (equal input '(GUERREROS DE LA FUERZA))
         (equal input '(QUIENES SON GUERREROS DE LA FUERZA))
         (equal input '(GUERREROS)))
     (mostrar-guerreros-fuerza))
    
    ((and (>= (length input) 5)
          (eq (first input) 'ES)
          (equal (subseq input 2 5) '(GUERRERO DE LA)))
     (verificar-guerrero-fuerza (second input)))
    
    ((and (>= (length input) 4)
          (eq (first input) 'ES)
          (equal (subseq input 2 4) '(UN GUERRERO)))
     (verificar-guerrero-fuerza (second input)))
    
    ;; Enemigos
    ((and (>= (length input) 3)
          (eq (first input) 'ENEMIGOS)
          (eq (second input) 'DE))
     (mostrar-enemigos (third input)))
    
    ((and (>= (length input) 5)
          (equal (subseq input 0 4) '(QUIENES SON ENEMIGOS DE)))
     (mostrar-enemigos (fifth input)))
    
    ;; Son enemigos X y Y
    ((and (>= (length input) 5)
          (eq (first input) 'SON)
          (eq (second input) 'ENEMIGOS)
          (eq (fourth input) 'Y))
     (verificar-enemigos (third input) (fifth input)))
    
    ((and (>= (length input) 5)
          (eq (first input) 'SON)
          (eq (third input) 'Y)
          (eq (fifth input) 'ENEMIGOS))
     (verificar-enemigos (second input) (fourth input)))
    
    ((and (>= (length input) 5)
          (eq (first input) 'ES)
          (eq (third input) 'ENEMIGO)
          (eq (fourth input) 'DE))
     (verificar-enemigos (second input) (fifth input)))
    
    ;; Compañeros
    ((and (>= (length input) 3)
          (eq (first input) 'COMPANEROS)
          (eq (second input) 'DE))
     (mostrar-companeros (third input)))
    
    ((and (>= (length input) 5)
          (equal (subseq input 0 4) '(QUIENES SON COMPANEROS DE)))
     (mostrar-companeros (fifth input)))
    
    ((and (>= (length input) 3)
          (eq (first input) 'EQUIPO)
          (eq (second input) 'DE))
     (mostrar-companeros (third input)))
    
    ;; Son compañeros X y Y
    ((and (>= (length input) 5)
          (eq (first input) 'SON)
          (eq (second input) 'COMPANEROS)
          (eq (fourth input) 'Y))
     (verificar-companeros (third input) (fifth input)))
    
    ((and (>= (length input) 5)
          (eq (first input) 'SON)
          (eq (third input) 'Y)
          (eq (fifth input) 'COMPANEROS))
     (verificar-companeros (second input) (fourth input)))
    
    ((and (>= (length input) 5)
          (eq (first input) 'ES)
          (eq (third input) 'COMPANERO)
          (eq (fourth input) 'DE))
     (verificar-companeros (second input) (fifth input)))
    
    ;; Por afiliación
    ((equal input '(JEDI))
     (mostrar-por-afiliacion 'jedi))
    ((equal input '(SITH))
     (mostrar-por-afiliacion 'sith))
    ((equal input '(REBELION))
     (mostrar-por-afiliacion 'rebelion))
    ((equal input '(CAZARRECOMPENSAS))
     (mostrar-por-afiliacion 'cazarrecompensas))
    
    ((and (>= (length input) 2)
          (eq (first input) 'LISTAR))
     (mostrar-por-afiliacion (second input)))
    
    ((and (>= (length input) 3)
          (equal (subseq input 0 2) '(QUIENES SON)))
     (mostrar-por-afiliacion (third input)))
    
    ;; Sensibles a la fuerza
    ((or (equal input '(SENSIBLES A LA FUERZA))
         (equal input '(QUIENES SON SENSIBLES A LA FUERZA))
         (equal input '(USUARIOS DE LA FUERZA)))
     (mostrar-sensibles-fuerza))
    
    ((and (>= (length input) 6)
          (eq (first input) 'ES)
          (equal (subseq input 2 6) '(SENSIBLE A LA FUERZA)))
     (verificar-sensible-fuerza (second input)))
    
    ((and (>= (length input) 5)
          (eq (first input) 'PUEDE)
          (equal (subseq input 2 5) '(USAR LA FUERZA)))
     (verificar-sensible-fuerza (second input)))
    
    ;; Ayuda
    ((equal input '(AYUDA))
     (format t "Comandos: personajes, info de X, guerreros de la fuerza,~%")
     (format t "          enemigos de X, companeros de X, jedi, sith~%"))
    
    ;; Default
    (t (format t "No entendi. Escribe 'ayuda' o 'menu' para volver.~%"))))

;;; back-end enfermedades

(defun procesar-medico (input)
  "Procesa comandos del módulo médico"
  (cond
    ((and (>= (length input) 1)
          (eq (first input) 'HOLA))
     (format t "Hola, dime tus sintomas~%"))
    
    ((equal input '(ENFERMEDADES))
     (format t "Enfermedades: epilepsia, gota, autoinmune~%"))
    
    ((and (>= (length input) 3)
          (eq (first input) 'SINTOMAS)
          (eq (second input) 'DE))
     (mostrar-sintomas-enfermedad (third input)))
    
    ((and (>= (length input) 3)
          (eq (first input) 'TRATAMIENTO)
          (eq (second input) 'PARA))
     (mostrar-tratamiento (third input)))
    
    ((and (>= (length input) 2)
          (member (first input) '(TENGO SIENTO TAMBIEN)))
     (agregar-sintoma-reportado (second input)))
    
    ((equal input '(MIS SINTOMAS))
     (mostrar-sintomas-reportados))
    
    ((or (equal input '(LISTO))
         (equal input '(SON TODOS))
         (equal input '(DIAGNOSTICO)))
     (realizar-diagnostico-auto))
    
    ((equal input '(REINICIAR))
     (limpiar-sintomas-reportados)
     (format t "Sintomas borrados.~%"))
    
    ;; Intenta agregar como síntoma si es conocido
    ((and (= (length input) 1)
          (es-sintoma-conocido-p (first input)))
     (agregar-sintoma-reportado (first input)))
    
    (t (format t "No entendi. Usa 'tengo [sintoma]' o 'menu'.~%"))))

;;; inicial

(defun iniciar-modulo-starwars ()
  "Star Wars"
  (format t "~%      STAR WARS~%")
  (format t "~%Comandos disponibles:~%")
  (format t "~%  Consultas basicas:~%")
  (format t "    personajes              - Lista todos los personajes~%")
  (format t "    especie de [nombre]     - Ver especie del personaje~%")
  (format t "    afiliacion de [nombre]  - Ver afiliacion del personaje~%")
  (format t "    arma de [nombre]        - Ver arma principal~%")
  (format t "    info de [nombre]        - Ver toda la info del personaje~%")
  (format t "~%  Consultas extra:~%")
  (format t "    sensibles a la fuerza   - Jedi o Sith~%")
  (format t "    enemigos de [nombre]    - Personajes enemigos~%")
  (format t "    companeros de [nombre]  - Compis~%")
  (format t "    son enemigos [X] y [Y]  - Verificar si son enemigos~%")
  (format t "    son companeros [X] y [Y]- Verificar si son companeros~%")
  (format t "~%  LISTAS:~%")
  (format t "    jedi / sith / rebelion  - Listar por afiliacion~%")
  (format t "~%  menu - Volver al menu principal~%~%")
  (eliza-starwars))

(defun eliza-starwars ()
  "Loop principal de Star Wars"
  (let ((input (leer-entrada)))
    (cond
      ((member (first input) '(MENU VOLVER SALIR))
       (setf *modo-actual* nil)
       (eliza))
      (t
       (procesar-starwars input)
       (eliza-starwars)))))

(defun iniciar-modulo-medico ()
  "Diagnostico de enfermedades"
  (limpiar-sintomas-reportados)
  (format t "~%    Diagnostico de enfermedades~%")
  (format t "~%Comandos:~%")
  (format t "  tengo [sintoma]   - Reportar sintoma~%")
  (format t "  listo             - Ver diagnostico~%")
  (format t "  mis sintomas      - Ver sintomas reportados~%")
  (format t "  sintomas de [enf] - Ver sintomas de enfermedad~%")
  (format t "  enfermedades      - Lista de enfermedades~%")
  (format t "  reiniciar         - Borrar sintomas~%")
  (format t "  menu              - Volver~%~%")
  (eliza-medico))

(defun eliza-medico ()
  "Loop principal médico"
  (let ((input (leer-entrada)))
    (cond
      ((member (first input) '(MENU VOLVER SALIR))
       (setf *modo-actual* nil)
       (eliza))
      (t
       (procesar-medico input)
       (eliza-medico)))))

;;; Menu principal

(defun menu-principal (input)
  "Procesa la opción del menú principal"
  (cond
    ((member (first input) '(SALIR ADIOS))
     (format t "Que la Fuerza te acompañe!~%"))
    
    ((equal input '(MEDICO))
     (setf *modo-actual* 'medico)
     (iniciar-modulo-medico))
    
    ((or (equal input '(STARWARS))
         (equal input '(STAR WARS)))
     (setf *modo-actual* 'starwars)
     (iniciar-modulo-starwars))
    
    (t
     (format t "Opcion no valida. Escribe 'medico', 'starwars' o 'salir'.~%")
     (menu-principal (leer-entrada)))))

(defun eliza ()
  "Función principal del programa"
  (limpiar-todo)
  (format t "~%     Proyecto final. NO ME REPRUEBE PROFE~%")
  (format t "~%Ingrese la opcion deseada:~%")
  (format t "~%  medico   - Diagnostico de enfermedades~%")
  (format t "  starwars - Base de datos Star Wars~%")
  (format t "  salir    - Terminar programa~%~%")
  (menu-principal (leer-entrada)))