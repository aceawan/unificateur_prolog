% Occur check
occur_check(V,T) :- var(V),compound(T), is_var_in_term(V,T). % Si T n'est ni une variable, ni un atome, il est composé, on vérifie si X apparait dans le terme

is_var_in_term(V,T) :- var(T), V == T.
is_var_in_term(V,T) :- compound(T), functor(T,_,A), check_nth_arg(V,T,1,A).

%check_nth_arg(V, T, K, K) :- arg(K,T,X), is_var_in_term(V,X).
%check_nth_arg(V, T, K, A) :- K \= A, arg(K,T,X), is_var_in_term(V,X).
%check_nth_arg(V, T, K, A) :- K \= A, plus(K,1,Y), check_nth_arg(V,T,Y,A).

check_nth_arg(V, T, A) :- A > 0, arg(A,T,X), is_var_in_term(V,X).
check_nth_arg(V, T, A) :- A \= 1, plus(A, -1, Y), check_nth_arg(V,T,Y).
