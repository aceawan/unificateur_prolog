:- style_check(-singleton).
% Opération ?=
:- op(20,xfy,?=).

% Occur check

% Si T est composé, il n'est pas une variable ou un atome, on vérifie si la variable V apparait dans T
occur_check(V,T) :- var(V),compound(T), not(is_var_in_term(V,T)).

% Si T et V sont identiques, la variable apparait dans le terme T
is_var_in_term(V,T) :- var(T), V == T.
% Si T est composé, on vérifie si la variable est un de ses arguments ou si elle apparait dans un de ses arguments
is_var_in_term(V,T) :- compound(T), functor(T,_,A), check_nth_arg(V,T,A).

% On itère sur les A arguments du terme T pour vérifier si la variable V apparait dans un argument de T.
check_nth_arg(V, T, A) :- A > 0, arg(A,T,X), is_var_in_term(V,X).
check_nth_arg(V, T, A) :- A \= 1, plus(A, -1, Y), check_nth_arg(V,T,Y).

% Règles
regle(X ?= Y, rename) :- var(X), var(Y), !.
regle(X ?= Y, simplify) :- atomic(Y), !.
regle(X ?= Y, expand) :- compound(Y), var(X), occur_check(X,Y), !.
regle(X ?= Y, orient) :- not(var(X)), var(Y), !.
regle(X ?= Y, decompose) :- compound(X), compound(Y), functor(X,N,A), functor(Y,M,B), (M == N), (A == B), !.
regle(X ?= Y, clash) :- not(compound(X)).
regle(X ?= Y, clash) :- not(compound(Y)).
regle(X ?= Y, clash) :- compound(X), compound(Y), functor(X,A,_), functor(Y,B,_), A \= B, !.
regle(X ?= Y, clash) :- compound(X), compound(Y), functor(X,_,N), functor(Y,_,M), N \= M, !.

% Réduction
reduit(rename, X ?= Y, P, Q) :- print("rename"), nl, Q = P, X = Y.
reduit(simplify, X ?= Y, P, Q) :- print("simplify"), nl, Q = P, X = Y.
reduit(expand, X ?= Y, P, Q) :- print("expand"), nl, Q = P, X = Y.
reduit(orient, X ?= Y, P, Q) :- print("orient"), nl, append(P, [Y ?=X], Q).
reduit(decompose, X ?= Y, P, Q) :- print("decompose"), nl, functor(X,_,A), decomposition(X,Y,A,R), append(R,P,Q).

% Décomposition des arguments d'une fonction en une liste d'équations
decomposition(X, Y, N, Q) :- N \= 1, plus(N, -1, M), decomposition(X, Y, M, P), arg(N, X, A), arg(N, Y, B), append([A ?= B], P, Q).
decomposition(X, Y, N, Q) :- N == 1, arg(N, X, A), arg(N, Y, B), Q = [A ?= B].

% Unification
unifie([]).
unifie([X|T]) :- print([X|T]), nl, regle(X, R), reduit(R, X, T, Q), unifie(Q).
