% 1.-
cabeza_y_cola([C|T], C, T).

% 2.-
pertenece(X, [X|_]).
pertenece(X, [_|T]) :- pertenece(X, T).

% 3.-
longitud([], 0).
longitud([_|T], N) :- longitud(T, N1), N is N1 + 1.

% 4.-
concatenar([], L, L).
concatenar([H|T], L, [H|R]) :- concatenar(T, L, R).

% 5.-
invertir([], []).
invertir([H|T], R) :- invertir(T, TInv), concatenar(TInv, [H], R).

% 6.-
ultimo([X], X).
ultimo([_|T], X) :- ultimo(T, X).

% 7.-
suma_lista([], 0).
suma_lista([H|T], S) :- suma_lista(T, S1), S is S1 + H.

% 8.-
eliminar(_, [], []).
eliminar(X, [X|T], T).
eliminar(X, [H|T], [H|R]) :- X \= H, eliminar(X, T, R).

% 9.-
duplicar([], []).
duplicar([H|T], [H,H|R]) :- duplicar(T, R).

% 10.-
intercalar([], L, L).
intercalar(L, [], L).
intercalar([H1|T1], [H2|T2], [H1,H2|R]) :- intercalar(T1, T2, R).