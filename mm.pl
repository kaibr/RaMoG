% Martelli-Montanari-Algorithmus.
% mgu(+T1,+T2,-MGU)


:- module(mgu,[mgu/3]).

:- use_module(subterm).
:- use_module(substitute).

mgu(T1,T2,MGU) :- mm([subst(T1,T2)],MGU,_Failure).

mm(Unsolved,Solved,Failure) :- not solved(Unsolved),
                               Failure \== failure,
			       member(Eq,Unsolved),
			       delete(Unsolved,Eq,UnsolvedRest),
			       (mmcase1(Eq,UnsolvedRest,New,Failure);
       			       mmcase2(Eq,UnsolvedRest,New,Failure);
			       mmcase3(Eq,UnsolvedRest,New,Failure);
			       mmcase4(Eq,UnsolvedRest,New,Failure);
			       mmcase5(Eq,UnsolvedRest,New,Failure);
			       mmcase6(Eq,UnsolvedRest,New,Failure)),
			       !,
			       mm(New,Solved,Failure).

mm(Solved,Solved,Failure) :- Failure \== failure,
	                     solved(Solved).

solved(L) :- var_term_form(L),
	     no_var_twice(L,L).

var_term_form([]).
var_term_form([subst(v(_),_)|T]) :- 	solved(T).

no_var_twice([],_).
no_var_twice([subst(v(N),Term)|T],L) :- 
	                       delete(L,subst(v(N),Term),RestL),
			       not subterm(v(N),RestL),
			       not subterm(v(N),Term),
			       no_var_twice(T,L).


% same functor
mmcase1(subst(L,R),UnsolvedRest,New,_Failure) :-
	                       L \= v(_),
			       R \= v(_),
                               L =.. [Lfunc|Largs],
			       R =.. [Rfunc|Rargs],
			       Lfunc == Rfunc,
			       doSplit(Largs,Rargs,Split),
			       append(UnsolvedRest,Split,New).

doSplit([],[],[]).
doSplit([HL|TL],[HR|TR],[subst(HL,HR)|TS]) :- doSplit(TL,TR,TS).

% different functor - "clash"
mmcase2(subst(L,R),_UnsolvedRest,_New,failure) :-
	                       L \= v(_),
			       R \= v(_),
	                       functor(L,FL,_),
			       functor(R,FR,_),
			       FL \== FR.

% x=x
mmcase3(subst(v(N),v(N)),UnsolvedRest,UnsolvedRest,_Failure).

% turn around t=x
mmcase4(subst(T,v(N)),UnsolvedRest,New,_Failure) :-
                               T \= v(_),
			       New = [subst(v(N),T)|UnsolvedRest].

% substitute
mmcase5(subst(v(N),T),UnsolvedRest,New,_Failure) :-
	                       not subterm(v(N),T),
                   subterm(v(N),UnsolvedRest),
			       substitute(v(N),T,UnsolvedRest,RestNew),
			       New = [subst(v(N),T)|RestNew].

% occur-check failure
mmcase6(subst(v(N),T),_UnsolvedRest,_New,failure) :-
                               subterm(v(N),T),
                               v(N) \== T.




                               









