% Opération ?=
:- op(20,xfy,?=).

% Occur check

% Si T est composé, il n'est pas une variable ou un atome, on vérifie si la variable V apparait dans T
occur_check(V,T) :- var(V),compound(T), is_var_in_term(V,T).

% Si T et V sont identiques, la variable apparait dans le terme T
is_var_in_term(V,T) :- var(T), V == T. 
% Si T est composé, on vérifie si la variable est un de ses arguments ou si elle apparait dans un de ses arguments
is_var_in_term(V,T) :- compound(T), functor(T,_,A), check_nth_arg(V,T,A). 

% On itère sur les A arguments du terme T pour vérifier si la variable V apparait dans un argument de T.
check_nth_arg(V, T, K, K) :- arg(K,T,X), is_var_in_term(V,X).
check_nth_arg(V, T, K, A) :- K \= A, arg(K,T,X), is_var_in_term(V,X).
check_nth_arg(V, T, K, A) :- K \= A, plus(K,1,Y), check_nth_arg(V,T,Y,A).
