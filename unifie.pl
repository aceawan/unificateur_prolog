% Occur check
% occur_check(_,T) :- atomic(T). % Si T est atomique, X ne peut pas apparaître dans T
% occur_check(_,T) :- var(T). % Si T est une variable, soi elle est différente de X et donc X n'apparait pas dans T, soit elle est identique à T et la règle d'occur check n'échoue pas
occur_check(V,T) :- var(V),compound(T), is_var_in_term(V,T). % Si T n'est ni une variable, ni un atome, il est composé, on vérifie si X apparait dans le terme

is_var_in_term(V,T) :- var(T), V == T.
is_var_in_term(V,T) :- compound(T), functor(T,_,A), check_nth_arg(V,T,1,A).

check_nth_arg(V, T, K, K) :- arg(K,T,X), is_var_in_term(V,X).
check_nth_arg(V, T, K, A) :- K \= A, arg(K,T,X), is_var_in_term(V,X). 
check_nth_arg(V, T, K, A) :- K \= A, plus(K,1,Y), check_nth_arg(V,T,Y,A).
