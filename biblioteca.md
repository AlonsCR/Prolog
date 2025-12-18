% 1.- La Biblioteca Nacional tiene el libro "Don Quijote de la Mancha".
tiene(biblioteca_nacional, don_quijote_de_la_mancha).

% 2.- Todos los libros en la Biblioteca Nacional están catalogados.
libro(don_quijote_de_la_mancha).
libro(cien_anios_de_soledad).
libro(el_principito).
libro(rayuela).
libro(la_odisea).

catalogado(Libro) :- tiene(biblioteca_nacional, Libro), libro(Libro).

% 3.- Existen libros que están en más de una biblioteca.
tiene(biblioteca_central, don_quijote_de_la_mancha).
tiene(biblioteca_central, cien_anios_de_soledad).
tiene(biblioteca_municipal, el_principito).

en_varias_bibliotecas(Libro) :-
    tiene(B1, Libro),
    tiene(B2, Libro),
    B1 \= B2.

% 4.- Si un libro es raro, entonces no se puede prestar.
raro(la_odisea).
raro(manuscrito_antiguo).

no_se_puede_prestar(Libro) :- raro(Libro).

% 5.- La Biblioteca Central tiene más de 10,000 libros.
cantidad_libros(biblioteca_central, 15000).

tiene_mas_de_10000(Biblioteca) :-
    cantidad_libros(Biblioteca, N),
    N > 10000.

% 6.- Todos los autores tienen al menos un libro en una biblioteca.
autor(cervantes).
autor(garcia_marquez).
autor(saint_exupery).
autor(cortazar).
autor(homero).

escribio(cervantes, don_quijote_de_la_mancha).
escribio(garcia_marquez, cien_anios_de_soledad).
escribio(saint_exupery, el_principito).
escribio(cortazar, rayuela).
escribio(homero, la_odisea).

autor_con_libro_en_biblioteca(Autor) :-
    autor(Autor),
    escribio(Autor, Libro),
    tiene(_, Libro).

% 7.- Existe un autor que tiene más de 5 libros publicados.
escribio(garcia_marquez, el_amor_en_tiempos_de_colera).
escribio(garcia_marquez, cronica_de_una_muerte_anunciada).
escribio(garcia_marquez, el_otono_del_patriarca).
escribio(garcia_marquez, el_coronel_no_tiene_quien_le_escriba).
escribio(garcia_marquez, doce_cuentos_peregrinos).

cantidad_libros_autor(Autor, N) :-
    autor(Autor),
    findall(L, escribio(Autor, L), Libros),
    length(Libros, N).

autor_con_mas_de_5_libros(Autor) :-
    cantidad_libros_autor(Autor, N),
    N > 5.

% 8.- No todos los libros de la biblioteca están en buen estado.
en_buen_estado(don_quijote_de_la_mancha).
en_buen_estado(cien_anios_de_soledad).
en_buen_estado(el_principito).
en_mal_estado(rayuela).
en_mal_estado(la_odisea).

no_todos_en_buen_estado(Biblioteca) :-
    tiene(Biblioteca, Libro),
    en_mal_estado(Libro).

% 9.- Si un libro está en buen estado, puede ser prestado.
puede_ser_prestado(Libro) :-
    libro(Libro),
    en_buen_estado(Libro),
    \+ raro(Libro).

% 10.- Todos los usuarios registrados pueden tomar prestado un libro.
usuario_registrado(juan).
usuario_registrado(maria).
usuario_registrado(pedro).
usuario_registrado(ana).

puede_tomar_prestado(Usuario, Libro) :-
    usuario_registrado(Usuario),
    puede_ser_prestado(Libro),
    \+ tiene_multa(Usuario).

% 11.- Existen libros que solo se pueden consultar en la biblioteca.
solo_consulta(manuscrito_antiguo).
solo_consulta(Libro) :- raro(Libro).

% 12.- Todo libro prestado debe ser devuelto en 15 días.
dias_devolucion(15).

prestamo(juan, don_quijote_de_la_mancha, fecha(2024,1,10)).

fecha_devolucion(Usuario, Libro, DiasMax) :-
    prestamo(Usuario, Libro, _),
    dias_devolucion(DiasMax).

% 13.- Hay un libro que nadie ha pedido en préstamo.
nunca_prestado(Libro) :-
    libro(Libro),
    \+ prestamo(_, Libro, _).

% 14.- Si un usuario tiene una multa, no puede pedir un libro prestado.
tiene_multa(pedro).

no_puede_pedir_prestado(Usuario) :- tiene_multa(Usuario).

% 15.- Todos los libros escritos por un mismo autor están en la misma sección.
seccion(cervantes, literatura_espanola).
seccion(garcia_marquez, literatura_latinoamericana).
seccion(saint_exupery, literatura_francesa).
seccion(cortazar, literatura_latinoamericana).
seccion(homero, clasicos).

seccion_libro(Libro, Seccion) :-
    escribio(Autor, Libro),
    seccion(Autor, Seccion).

% 16.- Existe un libro que tiene más de un ejemplar en la biblioteca.
ejemplares(don_quijote_de_la_mancha, biblioteca_nacional, 3).
ejemplares(cien_anios_de_soledad, biblioteca_central, 5).
ejemplares(el_principito, biblioteca_municipal, 2).

tiene_varios_ejemplares(Libro, Biblioteca) :-
    ejemplares(Libro, Biblioteca, N),
    N > 1.

% 17.- Todo usuario con más de tres préstamos debe devolver uno para pedir otro.
prestamos_activos(juan, 2).
prestamos_activos(maria, 4).
prestamos_activos(ana, 1).

debe_devolver_para_pedir(Usuario) :-
    usuario_registrado(Usuario),
    prestamos_activos(Usuario, N),
    N > 3.

puede_pedir_nuevo(Usuario) :-
    usuario_registrado(Usuario),
    prestamos_activos(Usuario, N),
    N =< 3,
    \+ tiene_multa(Usuario).

% 18.- Hay una sección de la biblioteca donde todos los libros son de ciencias.
seccion_biblioteca(ciencias).
seccion_biblioteca(literatura).
seccion_biblioteca(infantil).

libro_en_seccion(fisica_cuantica, ciencias).
libro_en_seccion(quimica_organica, ciencias).
libro_en_seccion(biologia_molecular, ciencias).

es_de_ciencias(Libro) :- libro_en_seccion(Libro, ciencias).

todos_son_ciencias(Seccion) :-
    Seccion = ciencias,
    \+ (libro_en_seccion(Libro, Seccion), \+ es_de_ciencias(Libro)).

% 19.- No todos los libros en la biblioteca tienen más de 100 páginas.
paginas(don_quijote_de_la_mancha, 863).
paginas(el_principito, 96).
paginas(cien_anios_de_soledad, 471).
paginas(rayuela, 600).

tiene_mas_de_100_paginas(Libro) :-
    paginas(Libro, N),
    N > 100.

tiene_menos_de_100_paginas(Libro) :-
    paginas(Libro, N),
    N =< 100.

no_todos_mas_de_100 :- tiene_menos_de_100_paginas(_).

% 20.- Existe un usuario que ha tomado prestados todos los libros de la sección infantil.
libro_en_seccion(el_principito, infantil).
libro_en_seccion(cuentos_grimm, infantil).
libro_en_seccion(pinocho, infantil).

ha_prestado(ana, el_principito).
ha_prestado(ana, cuentos_grimm).
ha_prestado(ana, pinocho).

todos_infantiles_prestados(Usuario) :-
    usuario_registrado(Usuario),
    \+ (libro_en_seccion(Libro, infantil), \+ ha_prestado(Usuario, Libro)).