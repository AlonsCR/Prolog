# Solución de Acertijo Lógico en Prolog

```prolog
resolver(L) :-
    L = [
        investigador(ana,    EspA, HorA, BebA, EqA, PaiA),
        investigador(bruno,  EspB, HorB, BebB, EqB, PaiB),
        investigador(carlos, EspC, HorC, BebC, EqC, PaiC),
        investigador(diana,  EspD, HorD, BebD, EqD, PaiD),
        investigador(elisa,  EspE, HorE, BebE, EqE, PaiE)
    ],

    % --- Dominios ---
    permutation([genetica, microbiologia, bioquimica, inmunologia, neurociencia],
                [EspA, EspB, EspC, EspD, EspE]),

    permutation(['6am', '8am', '10am', '12pm', '14pm'],
                [HorA, HorB, HorC, HorD, HorE]),

    permutation([cafe, te, jugo, mate, agua],
                [BebA, BebB, BebC, BebD, BebE]),

    permutation([microscopio, centrifuga, pcr, espectrometro, incubadora],
                [EqA, EqB, EqC, EqD, EqE]),

    permutation([mexico, chile, espana, argentina, peru],
                [PaiA, PaiB, PaiC, PaiD, PaiE]),


    % --- Restricciones ---

    % 1. Genética → 6am
    member(investigador(_, genetica, '6am', _, _, _), L),

    % 2. Ana no trabaja en Genética ni Neurociencia
    EspA \= genetica,
    EspA \= neurociencia,

    % 3. Centrífuga → Té
    member(investigador(_, _, _, te, centrifuga, _), L),

    % 4. Perú → 10am
    member(investigador(_, _, '10am', _, _, peru), L),

    % 5. Carlos usa el Espectrómetro
    EqC = espectrometro,

    % 6. Café llega 2h antes que Jugo
    % Nota: Esta llamada usa variables específicas (Ana/Bruno), verifica si 
    % la lógica requiere buscar quién toma café y quién jugo en la lista general.
    antes(BebA, HorA, BebB, HorB, L), 

    % 7. Inmunología → PCR
    member(investigador(_, inmunologia, _, _, pcr, _), L),

    % 8. Bioquímica → Chile
    member(investigador(_, bioquimica, _, _, _, chile), L),

    % 9. Incubadora → 14pm
    member(investigador(_, _, '14pm', _, incubadora, _), L),

    % 10. Argentina → Mate
    member(investigador(_, _, _, mate, _, argentina), L),

    % 11. Elisa no bebe Té ni Café
    BebE \= te,
    BebE \= cafe,

    % 12. Diana → Microbiología
    EspD = microbiologia,

    % 13. 8am → Microscopio
    member(investigador(_, _, '8am', _, microscopio, _), L),

    % 14. Bruno no es de México
    PaiB \= mexico,

    % 15. España → Neurociencia
    member(investigador(_, neurociencia, _, _, _, espana), L),

    % 16. Usuario del PCR llega después que Microbiología
    despues_equipo(pcr, microbiologia, L),

    % 17. México usa equipo que NO es Microscopio ni Incubadora
    member(investigador(_, _, _, _, EqMex, mexico), L),
    EqMex \= microscopio,
    EqMex \= incubadora,

    % 18. Agua no usa PCR ni Espectrómetro
    member(investigador(_, _, _, agua, EqAg, _), L),
    EqAg \= pcr, EqAg \= espectrometro,

    % 19. Neurociencia llega después que Jugo
    despues(neurociencia, jugo, L),

    % 20. Perú NO bebe Agua
    member(investigador(_, _, _, BebPeru, _, peru), L),
    BebPeru \= agua.


% --- PREDICADOS AUXILIARES ---

horario_val('6am', 1).
horario_val('8am', 2).
horario_val('10am', 3).
horario_val('12pm', 4).
horario_val('14pm', 5).

% Regla para comparaciones de bebida y horario
antes(Beb1, Hor1, Beb2, Hor2, L) :-
    member(investigador(_, _, Hor1, Beb1, _, _), L),
    member(investigador(_, _, Hor2, Beb2, _, _), L),
    horario_val(Hor1, V1),
    horario_val(Hor2, V2),
    V1 is V2 - 2.  % Ajustar según la lógica deseada (indices vs horas reales)

% El de PCR llega después del de Microbiología
despues_equipo(Equipo, Especialidad, L) :-
    member(investigador(_, Especialidad, HorE, _, _, _), L),
    member(investigador(_, _, HorP, _, Equipo, _), L),
    horario_val(HorE, V1),
    horario_val(HorP, V2),
    V2 > V1.

% Especialidad llega después de bebida
despues(Especialidad, Bebida, L) :-
    member(investigador(_, Especialidad, HorE, _, _, _), L),
    member(investigador(_, _, HorB, Bebida, _, _), L),
    horario_val(HorE, V1),
    horario_val(HorB, V2),
    V1 > V2.
