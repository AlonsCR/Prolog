%% verificar que los elementos son unicos
todos_distintos([]).
todos_distintos([H|T]) :- 
    \+ member(H, T), 
    todos_distintos(T).

%% Predicado principal para resolver el acertijo lógico
resolver(Solucion) :-
    Solucion = [
        inv(ana, EspA, HorA, BebA, EqA, PaisA),
        inv(bruno, EspB, HorB, BebB, EqB, PaisB),
        inv(carlos, EspC, HorC, BebC, EqC, PaisC),
        inv(diana, EspD, HorD, BebD, EqD, PaisD),
        inv(elisa, EspE, HorE, BebE, EqE, PaisE)
    ],
    
    Especialidades = [genetica, microbiologia, bioquimica, inmunologia, neurociencia],
    Horarios = [6, 8, 10, 12, 14],
    Bebidas = [cafe, te, jugo, mate, agua],
    Equipos = [microscopio, centrifuga, pcr, espectrometro, incubadora],
    Paises = [mexico, chile, espana, argentina, peru],
    
    % Pistas directas
    EspD = microbiologia,
    EqC = espectrometro,
    
    % Restricciones de Especialidades
    member(EspA, Especialidades),
    member(EspB, Especialidades),
    member(EspC, Especialidades),
    member(EspE, Especialidades),
    EspA \= genetica,
    EspA \= neurociencia,
    todos_distintos([EspA, EspB, EspC, EspD, EspE]),
    
    % Restricciones de Horarios
    member(HorA, Horarios),
    member(HorB, Horarios),
    member(HorC, Horarios),
    member(HorD, Horarios),
    member(HorE, Horarios),
    todos_distintos([HorA, HorB, HorC, HorD, HorE]),
    
    % Relaciones Horario/Equipo/Pais
    member(inv(_, genetica, 6, _, _, _), Solucion),
    member(inv(_, _, 8, _, microscopio, _), Solucion),
    member(inv(_, _, 10, _, _, peru), Solucion),
    member(inv(_, _, 14, _, incubadora, _), Solucion),
    
    % Restricciones de Bebidas
    member(BebA, Bebidas),
    member(BebB, Bebidas),
    member(BebC, Bebidas),
    member(BebD, Bebidas),
    member(BebE, Bebidas),
    BebE \= te,
    BebE \= cafe,
    todos_distintos([BebA, BebB, BebC, BebD, BebE]),
    
    % Relación Café y Jugo (Jugo es 2 horas después del Café)
    member(inv(_, _, HorCafe, cafe, _, _), Solucion),
    member(inv(_, _, HorJugo, jugo, _, _), Solucion),
    HorJugo is HorCafe + 2,
    
    % Restricciones de Equipos
    member(EqA, Equipos),
    member(EqB, Equipos),
    member(EqD, Equipos),
    member(EqE, Equipos),
    todos_distintos([EqA, EqB, EqC, EqD, EqE]),
    
    % Relaciones específicas de Equipos
    member(inv(_, _, _, te, centrifuga, _), Solucion),
    member(inv(_, inmunologia, _, _, pcr, _), Solucion),
    
    % El PCR se usa después del horario de Diana
    member(inv(_, _, HorPCR, _, pcr, _), Solucion),
    HorPCR > HorD,
    
    % Restricciones del Agua
    member(inv(_, _, _, agua, EqAgua, _), Solucion),
    EqAgua \= pcr,
    EqAgua \= espectrometro,
    
    % Restricciones de Países
    member(PaisA, Paises),
    member(PaisB, Paises),
    member(PaisC, Paises),
    member(PaisD, Paises),
    member(PaisE, Paises),
    PaisB \= mexico,
    todos_distintos([PaisA, PaisB, PaisC, PaisD, PaisE]),
    
    % Relaciones País/Especialidad/Bebida
    member(inv(_, bioquimica, _, _, _, chile), Solucion),
    member(inv(_, _, _, mate, _, argentina), Solucion),
    member(inv(_, neurociencia, _, _, _, espana), Solucion),
    
    % Restricciones de México
    member(inv(_, _, _, _, EqMex, mexico), Solucion),
    EqMex \= microscopio,
    EqMex \= incubadora,
    
    % Neurociencia es después del Jugo
    member(inv(_, neurociencia, HorNeuro, _, _, _), Solucion),
    HorNeuro > HorJugo,
    
    % El de Perú no bebe agua
    member(inv(_, _, _, BebPeru, _, peru), Solucion),
    BebPeru \= agua.

%% Predicados para imprimir los resultados
mostrar([]).
mostrar([inv(N,E,H,B,Eq,P)|T]) :-
    format('~w: ~w, ~w:00 hrs, ~w, ~w, ~w~n', [N,E,H,B,Eq,P]),
    mostrar(T).

%% Punto de entrada principal
main :-
    resolver(S),
    write('=== SOLUCION DEL ACERTIJO ==='), nl,
    mostrar(S).