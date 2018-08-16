% Most General Rational Unifier
% mgru(+RestrictedAtom1,+RestrictedAtom2,-RestrictedSubst)

:- module(mgru,[mgru/3]).

:- use_module(mgu).
:- use_module(applySubst).


mgru(restatom(A1,Restr1),restatom(A2,Restr2),restsubst(Subst,Restr)) :-
	mgu(A1,A2,Subst),
	applySubst(A1,Subst,A1Subst),
	sumOverRestr(A1Subst,Restr1,Sum1,[],A1),
	sumOverRestr(A1Subst,Restr2,Restr,Sum1,A2).

sumOverRestr(_,[],Restr,Restr,_).
sumOverRestr(A1Subst,[Theta|T],Restr,Acc,A) :-
	applySubst(A,Theta,ATheta),
        if_then_else( mgu(A1Subst,ATheta,MGU),
                      sumOverRestr(A1Subst,T,Restr,[MGU|Acc],A),
		      sumOverRestr(A1Subst,T,Restr,Acc,A)).


if_then_else(P,Q,_) :- P,!,Q.
if_then_else(_,_,R) :- R.





