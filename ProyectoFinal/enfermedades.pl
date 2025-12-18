
:- dynamic sintoma/2.
:- dynamic sintoma_negado/2.
:- dynamic sintoma_reportado/1.
:- dynamic modo_actual/1.

eliza :-
    limpiar_todo,
    writeln('     Proyecto final. NO ME REPRUEBE PROFE'),
    writeln(''),
    writeln('Ingrese la opcion deseada:'),
    writeln(''),
    writeln('  familia  - Consultar arbol genealogico'),
    writeln('  medico   - Diagnostico de enfermedades'),
    writeln('  starwars - Base de datos Star Wars'),
    writeln('  salir    - Terminar programa'),
    writeln(''),
    readln(Input),
    menu_principal(Input), !.

menu_principal([salir]) :-
    writeln('Que la Fuerza te acompañe!'), !.
menu_principal([adios]) :-
    writeln('Que la Fuerza te acompañe!'), !.

menu_principal([familia]) :-
    assertz(modo_actual(familia)),
    iniciar_modulo_familia, !.

menu_principal([medico]) :-
    assertz(modo_actual(medico)),
    iniciar_modulo_medico, !.

menu_principal([starwars]) :-
    assertz(modo_actual(starwars)),
    iniciar_modulo_starwars, !.

menu_principal([star, wars]) :-
    assertz(modo_actual(starwars)),
    iniciar_modulo_starwars, !.

menu_principal(_) :-
    writeln('Opcion no valida. Escribe "familia", "medico", "starwars" o "salir".'),
    readln(Input),
    menu_principal(Input), !.

% Limpiar todo al iniciar
limpiar_todo :-
    retractall(sintoma_reportado(_)),
    retractall(modo_actual(_)).

% MÓDULO STAR WARS

iniciar_modulo_starwars :-
    nl,
    writeln('      STAR WARS'),
    writeln(''),
    writeln('Comandos disponibles:'),
    writeln(''),
    writeln('  Consultas basicas:'),
    writeln('    personajes              - Lista todos los personajes'),
    writeln('    especie de [nombre]     - Ver especie del personaje'),
    writeln('    afiliacion de [nombre]  - Ver afiliacion del personaje'),
    writeln('    arma de [nombre]        - Ver arma principal'),
    writeln('    info de [nombre]        - Ver toda la info del personaje'),
    writeln(''),
    writeln('  Consultas extra:'),
    writeln('    sensibles a la fuerza  - Jedi o Shit'),
    writeln('    enemigos de [nombre]    - Personajes enemigos'),
    writeln('    companeros de [nombre]  - Compis'),
    writeln('    son enemigos [X] y [Y]  - Verificar si son enemigos'),
    writeln('    son companeros [X] y [Y]- Verificar si son companeros'),
    writeln(''),
    writeln('  LISTAS:'),
    writeln('    jedi / sith / rebelion  - Listar por afiliacion'),
    writeln('    sensibles a la fuerza   - Listar sensibles'),
    writeln(''),
    writeln('  menu - Volver al menu principal'),
    writeln(''),
    readln(Input),
    eliza_starwars(Input), !.

% Salir al menú principal
eliza_starwars([menu]) :-
    retractall(modo_actual(_)),
    eliza, !.
eliza_starwars([volver]) :-
    retractall(modo_actual(_)),
    eliza, !.
eliza_starwars([salir]) :-
    retractall(modo_actual(_)),
    eliza, !.

% Procesar entrada
eliza_starwars(Input) :-
    procesar_starwars(Input),
    readln(Input1),
    eliza_starwars(Input1), !.

% Proceso st

% Lista de personajes
procesar_starwars([personajes]) :-
    mostrar_personajes, !.

procesar_starwars([lista, personajes]) :-
    mostrar_personajes, !.

procesar_starwars([quienes, son, los, personajes]) :-
    mostrar_personajes, !.

% Informacion de personaje
procesar_starwars([info, de, Nombre]) :-
    mostrar_info_personaje(Nombre), !.

procesar_starwars([informacion, de, Nombre]) :-
    mostrar_info_personaje(Nombre), !.

procesar_starwars([quien, es, Nombre]) :-
    mostrar_info_personaje(Nombre), !.

procesar_starwars([dime, sobre, Nombre]) :-
    mostrar_info_personaje(Nombre), !.

% Especie
procesar_starwars([especie, de, Nombre]) :-
    consultar_especie(Nombre), !.

procesar_starwars([que, especie, es, Nombre]) :-
    consultar_especie(Nombre), !.

procesar_starwars([de, que, especie, es, Nombre]) :-
    consultar_especie(Nombre), !.

% Afiliación
procesar_starwars([afiliacion, de, Nombre]) :-
    consultar_afiliacion(Nombre), !.

procesar_starwars([a, que, pertenece, Nombre]) :-
    consultar_afiliacion(Nombre), !.

procesar_starwars([de, que, lado, esta, Nombre]) :-
    consultar_afiliacion(Nombre), !.

% Arma
procesar_starwars([arma, de, Nombre]) :-
    consultar_arma(Nombre), !.

procesar_starwars([que, arma, usa, Nombre]) :-
    consultar_arma(Nombre), !.

procesar_starwars([arma, principal, de, Nombre]) :-
    consultar_arma(Nombre), !.

% jedi y shit
procesar_starwars([guerreros, de, la, fuerza]) :-
    mostrar_guerreros_fuerza, !.

procesar_starwars([quienes, son, guerreros, de, la, fuerza]) :-
    mostrar_guerreros_fuerza, !.

procesar_starwars([guerreros]) :-
    mostrar_guerreros_fuerza, !.

procesar_starwars([es, Nombre, guerrero, de, la, fuerza]) :-
    verificar_guerrero_fuerza(Nombre), !.

procesar_starwars([es, Nombre, un, guerrero]) :-
    verificar_guerrero_fuerza(Nombre), !.

% enemigos
procesar_starwars([enemigos, de, Nombre]) :-
    mostrar_enemigos(Nombre), !.

procesar_starwars([quienes, son, enemigos, de, Nombre]) :-
    mostrar_enemigos(Nombre), !.

procesar_starwars([son, enemigos, X, y, Y]) :-
    verificar_enemigos(X, Y), !.

procesar_starwars([son, X, y, Y, enemigos]) :-
    verificar_enemigos(X, Y), !.

procesar_starwars([es, X, enemigo, de, Y]) :-
    verificar_enemigos(X, Y), !.

% compañeros
procesar_starwars([companeros, de, Nombre]) :-
    mostrar_companeros(Nombre), !.

procesar_starwars([quienes, son, companeros, de, Nombre]) :-
    mostrar_companeros(Nombre), !.

procesar_starwars([equipo, de, Nombre]) :-
    mostrar_companeros(Nombre), !.

procesar_starwars([son, companeros, X, y, Y]) :-
    verificar_companeros(X, Y), !.

procesar_starwars([son, X, y, Y, companeros]) :-
    verificar_companeros(X, Y), !.

procesar_starwars([es, X, companero, de, Y]) :-
    verificar_companeros(X, Y), !.

% bando
procesar_starwars([jedi]) :-
    mostrar_por_afiliacion(jedi), !.

procesar_starwars([sith]) :-
    mostrar_por_afiliacion(sith), !.

procesar_starwars([rebelion]) :-
    mostrar_por_afiliacion(rebelion), !.

procesar_starwars([cazarrecompensas]) :-
    mostrar_por_afiliacion(cazarrecompensas), !.

procesar_starwars([listar, Afiliacion]) :-
    mostrar_por_afiliacion(Afiliacion), !.

procesar_starwars([quienes, son, Afiliacion]) :-
    mostrar_por_afiliacion(Afiliacion), !.

% sensibles a la fuerza
procesar_starwars([sensibles, a, la, fuerza]) :-
    mostrar_sensibles_fuerza, !.

procesar_starwars([quienes, son, sensibles, a, la, fuerza]) :-
    mostrar_sensibles_fuerza, !.

procesar_starwars([usuarios, de, la, fuerza]) :-
    mostrar_sensibles_fuerza, !.

procesar_starwars([es, Nombre, sensible, a, la, fuerza]) :-
    verificar_sensible_fuerza(Nombre), !.

procesar_starwars([puede, Nombre, usar, la, fuerza]) :-
    verificar_sensible_fuerza(Nombre), !.

% Ayuda
procesar_starwars([ayuda]) :-
    writeln('Comandos: personajes, info de X, guerreros de la fuerza,'),
    writeln('          enemigos de X, companeros de X, jedi, sith'), !.

% Default
procesar_starwars(_) :-
    writeln('No entendi. Escribe "ayuda" o "menu" para volver.'), !.

% hechos star wars

% especie(Personaje, Especie)
especie(anakin, humano).
especie(yoda, desconocida).
especie(chewbacca, wookiee).
especie(r2d2, droide).
especie(c3po, droide).
especie(darth_maul, zabrak).
especie(ahsoka, togruta).
especie(boba_fett, humano).
especie(obi_wan, humano).
especie(sidious, humano).
especie(mace_windu, humano).
especie(han_solo, humano).

% afiliacion(Personaje, Organizacion)
afiliacion(anakin, sith).
afiliacion(yoda, jedi).
afiliacion(chewbacca, rebelion).
afiliacion(r2d2, rebelion).
afiliacion(c3po, rebelion).
afiliacion(darth_maul, sith).
afiliacion(ahsoka, jedi).
afiliacion(boba_fett, cazarrecompensas).
afiliacion(obi_wan, jedi).
afiliacion(sidious, sith).
afiliacion(mace_windu, jedi).
afiliacion(han_solo, rebelion).

% arma_principal(Personaje, Arma)
arma_principal(anakin, sable_de_luz).
arma_principal(yoda, sable_de_luz).
arma_principal(chewbacca, ballesta_laser).
arma_principal(r2d2, herramientas).
arma_principal(c3po, protocolo).
arma_principal(darth_maul, sable_doble).
arma_principal(ahsoka, sables_duales).
arma_principal(boba_fett, rifle_blaster).
arma_principal(obi_wan, sable_de_luz).
arma_principal(sidious, rayos_fuerza).
arma_principal(mace_windu, sable_de_luz).
arma_principal(han_solo, blaster_dl44).

% es_sensible_a_la_fuerza(Personaje)
es_sensible_a_la_fuerza(anakin).
es_sensible_a_la_fuerza(yoda).
es_sensible_a_la_fuerza(darth_maul).
es_sensible_a_la_fuerza(ahsoka).
es_sensible_a_la_fuerza(obi_wan).
es_sensible_a_la_fuerza(sidious).
es_sensible_a_la_fuerza(mace_windu).

% Reglas

% GUERRERO 
% Alguien sensible a la fuerza que usa sable de luz
usa_sable(Personaje) :-
    arma_principal(Personaje, Arma),
    (Arma = sable_de_luz ; Arma = sable_doble ; Arma = sables_duales).

guerrero_de_la_fuerza(Personaje) :-
    es_sensible_a_la_fuerza(Personaje),
    usa_sable(Personaje).

% enemigos
afiliacion_opuesta(jedi, sith).
afiliacion_opuesta(sith, jedi).
afiliacion_opuesta(rebelion, sith).
afiliacion_opuesta(sith, rebelion).

% Dos personajes son enemigos si son de bandos opuestos
son_enemigos(X, Y) :-
    X \= Y,
    afiliacion(X, AfilX),
    afiliacion(Y, AfilY),
    afiliacion_opuesta(AfilX, AfilY).

% compañeros
son_companeros(X, Y) :-
    X \= Y,
    afiliacion(X, Org),
    afiliacion(Y, Org).

% consultas star wars

mostrar_personajes :-
    writeln('Personajes Star Wars'),
    findall(P, especie(P, _), Personajes),
    forall(member(Pers, Personajes), (
        especie(Pers, Esp),
        afiliacion(Pers, Afil),
        write('  - '), write(Pers),
        write(' ('), write(Esp), write(', '), write(Afil), writeln(')')
    )).

mostrar_info_personaje(Nombre) :-
    especie(Nombre, _), !,
    write('  Personaje: '), writeln(Nombre),
    writeln('---'),
    (especie(Nombre, Esp) -> (write('  Especie: '), writeln(Esp)) ; true),
    (afiliacion(Nombre, Afil) -> (write('  Afiliacion: '), writeln(Afil)) ; true),
    (arma_principal(Nombre, Arma) -> (write('  Arma principal: '), writeln(Arma)) ; true),
    (es_sensible_a_la_fuerza(Nombre) -> 
        writeln('  Sensible a la Fuerza: SI') 
    ; 
        writeln('  Sensible a la Fuerza: NO')
    ),
    (guerrero_de_la_fuerza(Nombre) ->
        writeln('  Guerrero de la Fuerza: SI')
    ;
        writeln('  Guerrero de la Fuerza: NO')
    ),
    writeln('---').
mostrar_info_personaje(Nombre) :-
    write('No conozco a: '), writeln(Nombre),
    writeln('Escribe "personajes" para ver la lista.').

consultar_especie(Nombre) :-
    (especie(Nombre, Esp) ->
        write(Nombre), write(' es de especie: '), writeln(Esp)
    ;
        write('No conozco a: '), writeln(Nombre)
    ).

consultar_afiliacion(Nombre) :-
    (afiliacion(Nombre, Afil) ->
        write(Nombre), write(' pertenece a: '), writeln(Afil)
    ;
        write('No conozco a: '), writeln(Nombre)
    ).

consultar_arma(Nombre) :-
    (arma_principal(Nombre, Arma) ->
        write(Nombre), write(' usa: '), writeln(Arma)
    ;
        write('No conozco a: '), writeln(Nombre)
    ).

mostrar_guerreros_fuerza :-
    writeln('Guerreros de lal fuerza'),
    writeln('(Sensibles a la Fuerza que usan sable de luz)'),
    writeln(''),
    findall(G, guerrero_de_la_fuerza(G), Guerreros),
    (Guerreros \= [] ->
        forall(member(Guer, Guerreros), (
            afiliacion(Guer, Afil),
            arma_principal(Guer, Arma),
            write('  - '), write(Guer),
            write(' ['), write(Afil), write('] - '), writeln(Arma)
        ))
    ;
        writeln('No hay guerreros de la fuerza registrados.')
    ),
    writeln('--').

verificar_guerrero_fuerza(Nombre) :-
    (guerrero_de_la_fuerza(Nombre) ->
        write('SI, '), write(Nombre), writeln(' es un Guerrero de la Fuerza.'),
        writeln('  (Es sensible a la Fuerza y usa sable de luz)')
    ;
        (especie(Nombre, _) ->
            write('NO, '), write(Nombre), writeln(' NO es un Guerrero de la Fuerza.'),
            (es_sensible_a_la_fuerza(Nombre) ->
                writeln('  (Es sensible a la Fuerza pero no usa sable)')
            ;
                writeln('  (No es sensible a la Fuerza)')
            )
        ;
            write('No conozco a: '), writeln(Nombre)
        )
    ).

% enemigos
mostrar_enemigos(Nombre) :-
    especie(Nombre, _), !,
    afiliacion(Nombre, MiAfil),
    write('Enemigos de'), write(Nombre), write(' ('), write(MiAfil), writeln(') ==='),
    findall(E, son_enemigos(Nombre, E), Enemigos),
    (Enemigos \= [] ->
        forall(member(Enem, Enemigos), (
            afiliacion(Enem, AfilEnem),
            write('  - '), write(Enem), write(' ['), write(AfilEnem), writeln(']')
        ))
    ;
        writeln('  No tiene enemigos registrados.')
    ),
mostrar_enemigos(Nombre) :-
    write('No conozco a: '), writeln(Nombre).

verificar_enemigos(X, Y) :-
    (son_enemigos(X, Y) ->
        afiliacion(X, AfilX),
        afiliacion(Y, AfilY),
        write('SI, '), write(X), write(' y '), write(Y), writeln(' son enemigos.'),
        write('  '), write(X), write(' es '), writeln(AfilX),
        write('  '), write(Y), write(' es '), writeln(AfilY)
    ;
        (especie(X, _), especie(Y, _) ->
            write('NO, '), write(X), write(' y '), write(Y), writeln(' no son enemigos.')
        ;
            writeln('No conozco a uno o ambos personajes.')
        )
    ).

% compañeros
mostrar_companeros(Nombre) :-
    especie(Nombre, _), !,
    afiliacion(Nombre, MiAfil),
    write('=== COMPANEROS DE '), write(Nombre), write(' ('), write(MiAfil), writeln(') ==='),
    findall(C, son_companeros(Nombre, C), Companeros),
    (Companeros \= [] ->
        forall(member(Comp, Companeros), (
            write('  - '), writeln(Comp)
        ))
    ;
        writeln('  No tiene companeros registrados.')
    ),
mostrar_companeros(Nombre) :-
    write('No conozco a: '), writeln(Nombre).

verificar_companeros(X, Y) :-
    (son_companeros(X, Y) ->
        afiliacion(X, Org),
        write('Si, '), write(X), write(' y '), write(Y), writeln(' son compañeros.'),
        write('  Ambos pertenecen a: '), writeln(Org)
    ;
        (especie(X, _), especie(Y, _) ->
            afiliacion(X, AfilX),
            afiliacion(Y, AfilY),
            write('no, '), write(X), write(' y '), write(Y), writeln(' no son compañeros.'),
            write('  '), write(X), write(' es '), writeln(AfilX),
            write('  '), write(Y), write(' es '), writeln(AfilY)
        ;
            writeln('No conozco a uno o ambos personajes.')
        )
    ).

%listas
mostrar_por_afiliacion(Afiliacion) :-
    findall(P, afiliacion(P, Afiliacion), Personajes),
    (Personajes \= [] ->
        write('Miembros de '), write(Afiliacion), writeln(' ==='),
        forall(member(Pers, Personajes), (
            write('  - '), writeln(Pers)
        ))
    ;
        write('No hay personajes en: '), writeln(Afiliacion)
    ).

mostrar_sensibles_fuerza :-
    writeln('Sensibles a la fuerza'),
    findall(P, es_sensible_a_la_fuerza(P), Sensibles),
    forall(member(Sens, Sensibles), (
        afiliacion(Sens, Afil),
        write('  - '), write(Sens), write(' ['), write(Afil), writeln(']')
    )),
    writeln('---').

verificar_sensible_fuerza(Nombre) :-
    (es_sensible_a_la_fuerza(Nombre) ->
        write('Si, '), write(Nombre), writeln(' es sensible a la Fuerza.')
    ;
        (especie(Nombre, _) ->
            write('No, '), write(Nombre), writeln(' No es sensible a la Fuerza.')
        ;
            write('No conozco a: '), writeln(Nombre)
        )
    ).

% arbol genealogico 

iniciar_modulo_familia :-
    writeln('     Arbol genealogico'),
    writeln(''),
    writeln('Comandos disponibles:'),
    writeln('  quien es el padre de [nombre]'),
    writeln('  quien es la madre de [nombre]'),
    writeln('  quienes son los hijos de [nombre]'),
    writeln('  quienes son los hermanos de [nombre]'),
    writeln('  quienes son los abuelos de [nombre]'),
    writeln('  quienes son los tios de [nombre]'),
    writeln('  quienes son los primos de [nombre]'),
    writeln('  es [X] padre de [Y]'),
    writeln('  mostrar arbol'),
    writeln('  miembros'),
    writeln('  menu - Volver al menu principal'),
    writeln(''),
    readln(Input),
    eliza_familia(Input), !.

% Salir al menú principal
eliza_familia([menu]) :- retractall(modo_actual(_)), eliza, !.
eliza_familia([volver]) :- retractall(modo_actual(_)), eliza, !.
eliza_familia([salir]) :- retractall(modo_actual(_)), eliza, !.

eliza_familia(Input) :-
    procesar_familia(Input),
    readln(Input1),
    eliza_familia(Input1), !.

% logica familia

procesar_familia([miembros]) :- 
    writeln('Miembros'),
    writeln('Hombres:'), forall(hombre(X), (write('  - '), writeln(X))),
    writeln('Mujeres:'), forall(mujer(X), (write('  - '), writeln(X))), !.

procesar_familia([mostrar, arbol]) :- mostrar_arbol_completo, !.
procesar_familia([arbol]) :- mostrar_arbol_completo, !.

procesar_familia([quien, es, el, padre, de, N]) :- consultar_padre(N), !.
procesar_familia([padre, de, N]) :- consultar_padre(N), !.

procesar_familia([quien, es, la, madre, de, N]) :- consultar_madre(N), !.
procesar_familia([madre, de, N]) :- consultar_madre(N), !.

procesar_familia([quienes, son, los, hijos, de, N]) :- consultar_hijos(N), !.
procesar_familia([hijos, de, N]) :- consultar_hijos(N), !.

procesar_familia([quienes, son, los, hermanos, de, N]) :- consultar_hermanos(N), !.
procesar_familia([hermanos, de, N]) :- consultar_hermanos(N), !.

procesar_familia([quienes, son, los, abuelos, de, N]) :- consultar_abuelos(N), !.
procesar_familia([abuelos, de, N]) :- consultar_abuelos(N), !.

procesar_familia([quienes, son, los, tios, de, N]) :- consultar_tios(N), !.
procesar_familia([tios, de, N]) :- consultar_tios(N), !.

procesar_familia([quienes, son, los, primos, de, N]) :- consultar_primos(N), !.
procesar_familia([primos, de, N]) :- consultar_primos(N), !.

procesar_familia([es, X, padre, de, Y]) :- verificar_padre(X, Y), !.
procesar_familia([es, X, madre, de, Y]) :- verificar_madre(X, Y), !.
procesar_familia([es, X, hermano, de, Y]) :- verificar_hermano(X, Y), !.
procesar_familia([es, X, abuelo, de, Y]) :- verificar_abuelo(X, Y), !.

procesar_familia([ayuda]) :-
    writeln('Comandos: padre de X, madre de X, hermanos de X, abuelos de X'), !.

procesar_familia(_) :-
    writeln('No entendi. Escribe "ayuda" o "menu".'), !.

% factos familia

hombre(pedro). hombre(juan). hombre(luis). hombre(diego). hombre(mateo).
mujer(maria). mujer(elena). mujer(carmen). mujer(ana). mujer(lucia).

progenitor(pedro, juan). progenitor(pedro, luis).
progenitor(maria, juan). progenitor(maria, luis).
progenitor(juan, ana). progenitor(juan, diego).
progenitor(elena, ana). progenitor(elena, diego).
progenitor(luis, mateo). progenitor(luis, lucia).
progenitor(carmen, mateo). progenitor(carmen, lucia).

% reglas familia

padre(P, H) :- hombre(P), progenitor(P, H).
madre(M, H) :- mujer(M), progenitor(M, H).
hermanos(X, Y) :- progenitor(P, X), progenitor(P, Y), X \= Y.
abuelo(A, N) :- hombre(A), progenitor(A, P), progenitor(P, N).
abuela(A, N) :- mujer(A), progenitor(A, P), progenitor(P, N).
tio(T, S) :- hombre(T), progenitor(P, S), hermanos(T, P).
tia(T, S) :- mujer(T), progenitor(P, S), hermanos(T, P).
primo(P, X) :- progenitor(PP, P), progenitor(PX, X), hermanos(PP, PX), P \= X.

% Funciones familia

consultar_padre(N) :- (padre(P, N) -> (write('Padre: '), writeln(P)) ; writeln('No encontrado.')).
consultar_madre(N) :- (madre(M, N) -> (write('Madre: '), writeln(M)) ; writeln('No encontrada.')).

consultar_hijos(N) :-
    findall(H, progenitor(N, H), Hijos),
    (Hijos \= [] -> (writeln('Hijos:'), forall(member(Hi,Hijos),(write('  - '),writeln(Hi)))) ; writeln('Sin hijos.')).

consultar_hermanos(N) :-
    findall(H, hermanos(H, N), L), sort(L, Hermanos),
    (Hermanos \= [] -> (writeln('Hermanos:'), forall(member(He,Hermanos),(write('  - '),writeln(He)))) ; writeln('Sin hermanos.')).

consultar_abuelos(N) :-
    findall(A, (abuelo(A,N);abuela(A,N)), L), sort(L, Abuelos),
    (Abuelos \= [] -> (writeln('Abuelos:'), forall(member(Ab,Abuelos),(write('  - '),writeln(Ab)))) ; writeln('Sin abuelos.')).

consultar_tios(N) :-
    findall(T, (tio(T,N);tia(T,N)), L), sort(L, Tios),
    (Tios \= [] -> (writeln('Tios:'), forall(member(Ti,Tios),(write('  - '),writeln(Ti)))) ; writeln('Sin tios.')).

consultar_primos(N) :-
    findall(P, primo(P, N), L), sort(L, Primos),
    (Primos \= [] -> (writeln('Primos:'), forall(member(Pr,Primos),(write('  - '),writeln(Pr)))) ; writeln('Sin primos.')).

verificar_padre(X, Y) :- (padre(X,Y) -> (write('SI, '),write(X),write(' es padre de '),writeln(Y)) ; (write('NO, '),write(X),write(' no es padre de '),writeln(Y))).
verificar_madre(X, Y) :- (madre(X,Y) -> (write('SI')) ; (write('NO'))), nl.
verificar_hermano(X, Y) :- (hermanos(X,Y) -> (write('SI')) ; (write('NO'))), nl.
verificar_abuelo(X, Y) :- (abuelo(X,Y) -> (write('SI')) ; (write('NO'))), nl.

% deteccion de enfermedades

iniciar_modulo_medico :-
    limpiar_sintomas_reportados,
    nl,
    writeln('    Diagnostico de enfermedades'),
    writeln(''),
    writeln('Comandos:'),
    writeln('  tengo [sintoma]   - Reportar sintoma'),
    writeln('  listo             - Ver diagnostico'),
    writeln('  mis sintomas      - Ver sintomas reportados'),
    writeln('  sintomas de [enf] - Ver sintomas de enfermedad'),
    writeln('  enfermedades      - Lista de enfermedades'),
    writeln('  reiniciar         - Borrar sintomas'),
    writeln('  menu              - Volver'),
    writeln(''),
    readln(Input),
    eliza_medico(Input), !.

eliza_medico([menu]) :- retractall(modo_actual(_)), eliza, !.
eliza_medico([volver]) :- retractall(modo_actual(_)), eliza, !.
eliza_medico([salir]) :- retractall(modo_actual(_)), eliza, !.

eliza_medico(Input) :-
    procesar_medico(Input),
    readln(Input1),
    eliza_medico(Input1), !.

% logica diagnostico 

procesar_medico([hola|_]) :- writeln('Hola, dime tus sintomas'), !.

procesar_medico([enfermedades]) :-
    writeln('Enfermedades: epilepsia, gota, autoinmune'), !.

procesar_medico([sintomas, de, E|_]) :- mostrar_sintomas(E), !.

procesar_medico([tratamiento, para, E|_]) :- mostrar_tratamiento(E), !.

procesar_medico([tengo, S|_]) :- agregar_sintoma_reportado(S), !.
procesar_medico([siento, S|_]) :- agregar_sintoma_reportado(S), !.
procesar_medico([tambien, S|_]) :- agregar_sintoma_reportado(S), !.

procesar_medico([S]) :- es_sintoma_conocido(S), agregar_sintoma_reportado(S), !.

procesar_medico([mis, sintomas|_]) :- mostrar_sintomas_reportados, !.

procesar_medico([listo|_]) :- realizar_diagnostico_auto, !.
procesar_medico([son, todos|_]) :- realizar_diagnostico_auto, !.
procesar_medico([diagnostico|_]) :- realizar_diagnostico_auto, !.

procesar_medico([reiniciar|_]) :- limpiar_sintomas_reportados, writeln('Sintomas borrados.'), !.

procesar_medico([Palabra|_]) :- es_sintoma_conocido(Palabra), agregar_sintoma_reportado(Palabra), !.

procesar_medico(_) :- writeln('No entendi. Usa "tengo [sintoma]" o "menu".'), !.

% factos medicos

tiene_sintoma(epilepsia, convulsiones).
tiene_sintoma(epilepsia, perdida_conciencia).
tiene_sintoma(epilepsia, confusion_temporal).
tiene_sintoma(epilepsia, aura_visual).
tiene_sintoma(epilepsia, mirada_fija).
tiene_sintoma(epilepsia, rigidez_muscular).
tiene_sintoma(epilepsia, mordedura_lengua).

tiene_sintoma(gota, dolor_articular_agudo).
tiene_sintoma(gota, hinchazon_dedo_gordo).
tiene_sintoma(gota, enrojecimiento_piel).
tiene_sintoma(gota, calor_zona_afectada).
tiene_sintoma(gota, sensibilidad_tacto).
tiene_sintoma(gota, nodulos_piel).
tiene_sintoma(gota, movilidad_limitada).

tiene_sintoma(autoinmune, fatiga_cronica).
tiene_sintoma(autoinmune, dolor_muscular_general).
tiene_sintoma(autoinmune, erupcion_cutanea).
tiene_sintoma(autoinmune, fiebre_recurrente).
tiene_sintoma(autoinmune, perdida_cabello).
tiene_sintoma(autoinmune, inflamacion_ganglios).
tiene_sintoma(autoinmune, entumecimiento_manos).

tratamiento(epilepsia, 'Antiepilepticos, dieta cetogenica, evitar luces estroboscopicas.').
tratamiento(gota, 'Colchicina, corticoides, hidratacion, dieta baja en purinas.').
tratamiento(autoinmune, 'Inmunosupresores, terapia biologica, antiinflamatorios.').

% funciones diagnostico

es_sintoma_conocido(S) :- tiene_sintoma(_, S).

agregar_sintoma_reportado(S) :-
    (es_sintoma_conocido(S) ->
        (sintoma_reportado(S) ->
            write('Ya reportaste: '), writeln(S)
        ;
            assertz(sintoma_reportado(S)),
            write('Registrado: '), writeln(S),
            findall(E, tiene_sintoma(E, S), Enfs),
            write('Asociado a: '), writeln(Enfs),
            writeln('Otro sintoma? o "listo" para diagnostico.')
        )
    ;
        write('No reconozco: '), writeln(S)
    ).

mostrar_sintomas_reportados :-
    findall(S, sintoma_reportado(S), L),
    (L = [] -> writeln('Sin sintomas.') ; (writeln('Tus sintomas:'), forall(member(X,L),(write('  - '),writeln(X))))).

limpiar_sintomas_reportados :- retractall(sintoma_reportado(_)).

realizar_diagnostico_auto :-
    findall(S, sintoma_reportado(S), Lista),
    (Lista = [] ->
        writeln('No reportaste sintomas.')
    ;
        nl, writeln('Diagnostico: '),
        writeln('Sintomas:'), forall(member(X,Lista),(write('  * '),writeln(X))), nl,
        analizar_por_enfermedad(epilepsia, Lista, C1),
        analizar_por_enfermedad(gota, Lista, C2),
        analizar_por_enfermedad(autoinmune, Lista, C3),
        nl, determinar_diagnostico_final([(epilepsia,C1),(gota,C2),(autoinmune,C3)])
    ).

analizar_por_enfermedad(E, Reportados, C) :-
    findall(S, tiene_sintoma(E,S), Sintomas),
    intersection(Reportados, Sintomas, Comunes),
    length(Comunes, C), length(Sintomas, T),
    write('  '), write(E), write(': '), write(C), write('/'), write(T),
    (C >= 3 -> write(' psoible') ; true), nl.

determinar_diagnostico_final(Lista) :-
    findall(C, member((_,C),Lista), Counts), max_list(Counts, Max),
    (Max >= 3 ->
        findall(E, member((E,Max),Lista), [Enf|_]),
        write('Diagnostico: '), writeln(Enf),
        tratamiento(Enf, Trat), write('Tratamiento: '), writeln(Trat)
    ;
        writeln('Sintomas insuficientes. Consulte a un medico.')
    ).

mostrar_sintomas(E) :-
    (tiene_sintoma(E,_) ->
        write('Sintomas de '), write(E), writeln(':'),
        forall(tiene_sintoma(E,S),(write('  - '),writeln(S)))
    ;
        write('No conozco: '), writeln(E)
    ).

mostrar_tratamiento(E) :-
    (tratamiento(E,T) -> (write('Tratamiento: '), writeln(T)) ; (write('No conozco: '), writeln(E))).