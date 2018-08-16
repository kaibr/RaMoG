% Martelli-Montanari-Algorithm.
% most general unifier that satisfies the following
% conditions:
% dom(mgu) contains all vars from T1 and T2
% all vars in ran(mgu) are fresh

% mgu(+T1,+T2,-MGU)


:- module(mgu,[mgu/3]).

:- use_module(subterm).
:- use_module(substitute).
:- use_module(rename).

mgu(T1,T2,MGU) :- 
	bagof(N,subterm(v(N),[T1,T2]),Vars),
	mm([subst(T1,T2)],MGU1,_Failure,Vars),
	rename_range(Vars,MGU1,MGU).
        
mm(Unsolved,Solved,Failure,Vars) :- 
	not solved(Unsolved,Vars),
        Failure \== failure,
	member(Eq,Unsolved),
	delete(Unsolved,Eq,UnsolvedRest),
	(mmcase1(Eq,UnsolvedRest,New,Failure);
       	 mmcase2(Eq,UnsolvedRest,New,Failure);
	 mmcase31(Eq,UnsolvedRest,New,Failure,Vars);
	 mmcase32(Eq,UnsolvedRest,New,Failure,Vars);
	 mmcase4(Eq,UnsolvedRest,New,Failure);
	 mmcase5(Eq,UnsolvedRest,New,Failure);
	 mmcase6(Eq,UnsolvedRest,New,Failure)),
	!,
	mm(New,Solved,Failure,Vars).

mm(Solved,Solved,Failure,Vars) :- Failure \== failure,
	                          solved(Solved,Vars).

solved(L,Vars) :- 
	var_term_form(L),
	no_var_twice(L,L),
	no_old_var_as_range(L,Vars).

var_term_form([]).
var_term_form([subst(v(_),_)|T]) :- 	var_term_form(T).

no_var_twice([],_).
no_var_twice([subst(v(N),Term)|T],L) :- 
	                       delete(L,subst(v(N),Term),RestL),
			       not subterm(v(N),RestL),
			       not subterm(v(N),Term),
			       no_var_twice(T,L).

no_old_var_as_range([],_).
no_old_var_as_range([subst(_L,R)|T],Vars) :- 
	not is_old_var(R,Vars),
	no_old_var_as_range(T,Vars).
is_old_var(v(N),Vars) :- member(N,Vars).

rename_range(Vars,Subst,NewSubst) :- 
	construct_list_of_rhs(Subst,Rhs),
	bagof(N,subterm(v(N),Rhs),VarsRhs),
	rename(Rhs,RhsRenamed),
	replace_rhs(Subst,RhsRenamed,RenSubst),
	bagof(N,subterm(v(N),RhsRenamed),VarsRhsRenamed),
	construct_bindings(Vars,VarsRhs,VarsRhsRenamed,BindingsList),
	list_to_set(BindingsList,Bindings),
	append(RenSubst,Bindings,NewSubst).

construct_list_of_rhs([],[]).
construct_list_of_rhs([subst(_L,R)|Subst],[R|Rhs]) :-
	construct_list_of_rhs(Subst,Rhs).

replace_rhs([],[],[]).
replace_rhs([subst(L,_R)|Subst],[Rren|RhsRenamed],[subst(L,Rren)|RenSubst]) :-
	replace_rhs(Subst,RhsRenamed,RenSubst).

construct_bindings(_,[],[],[]).
construct_bindings(Vars,[N|VarsR],[M|VarsRren],[subst(v(N),v(M))|Bindings]) :-
	member(N,Vars),
	construct_bindings(Vars,VarsR,VarsRren,Bindings).
construct_bindings(Vars,[N|VarsR],[_M|VarsRren],Bindings) :-
	not member(N,Vars),
	construct_bindings(Vars,VarsR,VarsRren,Bindings).

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

% x=x (old vars)
mmcase31(subst(v(N),v(N)),UnsolvedRest,New,_Failure,Vars) :-
	member(N,Vars),
	rename(v(0),V),
	New = [subst(v(N),V)|UnsolvedRest].

% x=x (new vars)
mmcase32(subst(v(N),v(N)),UnsolvedRest,UnsolvedRest,_Failure,Vars) :-
	not member(N,Vars).
	
% turn around t=x
mmcase4(subst(T,v(N)),UnsolvedRest,New,_Failure) :-
                               T \= v(_),
			       New = [subst(v(N),T)|UnsolvedRest].

% substitute Term
mmcase5(subst(v(N),T),UnsolvedRest,New,_Failure) :-
	not subterm(v(N),T),
	subterm(v(N),UnsolvedRest),
	substitute(v(N),T,UnsolvedRest,RestNew),
	New = [subst(v(N),T)|RestNew].

% occur-check failure
mmcase6(subst(v(N),T),_UnsolvedRest,_New,failure) :-
                               subterm(v(N),T),
                               v(N) \== T.




                               









