:- style_check(-singleton).
:- dynamic echo_on/0.
:- use_module(library(lists)).
:- set_prolog_flag(double_quotes, string).

% Opération ?=
:- op(20,xfy,?=).

%Prédicats d'affichage fournis

% set_echo: ce prédicat active l'affichage par le prédicat echo
set_echo :- assert(echo_on).

%clr_echo: ce prédicat inhibe l'affichage par le prédicat echo
clr_echo :- retractall(echo_on).

% echo(T): si le flag echo_on est positionné, echo(T) affiche le terme T
%          sinon, echo(T) réussit simplement en ne faisant rien.
echo(T) :- echo_on, !, write(T).
echo(_).
echoln(T) :- echo_on, echo(T), nl.
echoln(_).

% Occur check

% Si T est composé, il n'est pas une variable ou un atome, on vérifie si la variable V apparait dans T
occur_check(V,T) :- var(V),compound(T), not(is_var_in_term(V,T)).

% Condition d'arrêt de la récursion: T n'est plus qu'une simple variable. Si T et V sont identiques, la variable apparait dans le terme T
is_var_in_term(V,T) :- var(T), V == T.
% Si T est composé, on parcoure récursivement ses arguments pour vérifier la variable y apparait.
is_var_in_term(V,T) :- compound(T), functor(T,_,A), check_nth_arg(V,T,A).

% On itère sur les A arguments du terme T pour vérifier si la variable V apparait dans un argument de T.
check_nth_arg(V, T, A) :- A > 0, arg(A,T,X), is_var_in_term(V,X).
check_nth_arg(V, T, A) :- A \= 1, plus(A, -1, Y), check_nth_arg(V,T,Y).

% Le prédicat regle se satisfait lorsqu'une règle R peut s'appliquer à l'équation E.
regle(X ?= Y, rename) :- var(X), var(Y), !.
regle(X ?= Y, simplify) :- var(X), atomic(Y), !.
regle(X ?= Y, expand) :- compound(Y), var(X), occur_check(X,Y), !.
regle(X ?= Y, orient) :- not(var(X)), var(Y), !.
regle(X ?= Y, decompose) :- compound(X), compound(Y), functor(X,N,A), functor(Y,M,B), (M == N), (A == B), !.
regle(X ?= Y, clash) :- compound(X), compound(Y), functor(X,A,_), functor(Y,B,_), A \== B, !.
regle(X ?= Y, clash) :- compound(X), compound(Y), functor(X,_,N), functor(Y,_,M), N \== M, !.
regle(X ?= Y, occur_check) :- X \== Y, var(X), compound(Y), not(occur_check(X, Y)), !.
regle(X ?= Y, clean) :- atomic(X), atomic(Y), X == Y, !.

% Réduction
reduit(rename, X ?= Y, P, Q) :- echo("rename : "), echoln(X ?= Y), Q = P, X = Y, !.
reduit(simplify, X ?= Y, P, Q) :- echo("simplify : "), echoln(X ?= Y), Q = P, X = Y, !.
reduit(expand, X ?= Y, P, Q) :- echo("expand : "), echoln(X ?= Y), Q = P, X = Y, !.
reduit(orient, X ?= Y, P, Q) :- echo("orient : "), echoln(X ?= Y), append(P, [Y ?= X], Q), !.
reduit(decompose, X ?= Y, P, Q) :- echo("decompose : "), echoln(X ?= Y), functor(X,_,A), decomposition(X,Y,A,R), append(R,P,Q), !.
reduit(clean, X ?= Y, P, Q) :- echo("clean : "), echoln(X ?= Y), Q = P, !.

% Décomposition des arguments d'une fonction en une liste d'équations
decomposition(X, Y, N, Q) :- N \= 1, plus(N, -1, M), decomposition(X, Y, M, P), arg(N, X, A), arg(N, Y, B), append([A ?= B], P, Q).
decomposition(X, Y, N, Q) :- N == 1, arg(N, X, A), arg(N, Y, B), Q = [A ?= B].

% Stratégies de choix

% Choix de la première équation
choix_premier([X|T], Q, E, R) :- Q = T, E = X, regle(E, R), !.

% Choix en fonction des règles, fournies dans l'ordre suivant
% clash, check > rename, simplify > orient > decompose > expand
choix_pondere(P, Q, E, R) :- cherche_regle(P, [clash, occur_check, rename, simplify, orient, decompose, expand, clean], R, E), delete(P, E, Q), !.

% Cherche la règle R qu'on peut appliquer à E dans une liste de règles D qu'on peut appliquer à une liste d'équations d'unification L
cherche_regle(L, [X|D], R, E) :- cherche_elem(L, X, E), R = X, !.
cherche_regle(L, [X|D], R, E) :- cherche_regle(L, D, R, E), !.

% Cherche l'élément E sur lequel on peut appliquer une règle R dans une liste d'équations d'unification L.
cherche_elem([X|L], R, E) :- regle(X, R), E = X, !.
cherche_elem([X|L], R, E) :- cherche_elem(L,R,E), !.

% Unification
unifie([]).
unifie([X|T]) :- regle(X, clash), echo("clash : "), echoln(X), !, fail.
unifie([X|T]) :- regle(X, occur_check), echo("occur_check : "), echoln(X), !, fail.
unifie([X|T]) :- echo("system : "), echoln([X|T]), !, regle(X, R), reduit(R, X, T, Q), unifie(Q), !.

unifie([],S).
unifie(P,S) :- call(S, P, Q, X, R), R == clash, echo("clash : "), echoln(X), !, fail.
unifie(P,S) :- call(S, P, Q, X, R), R == occur_check, echo("occur_check : "), echoln(X), !, fail.
unifie(P,S) :- echo("system: "), echoln(P), !, call(S, P, Q, X, R), reduit(R, X, Q, F), unifie(F,S), !.

% Unification avec niveau de details
unif(P,S) :- clr_echo, unifie(P,S).
trace_unif(P,S) :- set_echo, unifie(P,S).
