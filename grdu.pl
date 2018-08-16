% General Rational Disunifier
% grdu(+Atom,+RestrictedAtom,-RestrictedSubst)

:- module(grdu,[grdu/3]).

:- use_module(mgu).
:- use_module(applySubst).


grdu(A1,restatom(A2,_Restr),restsubst([],Sigma)) :-
	if_then_else( mgu(A1,A2,MGU),
	              Sigma = [MGU],
		      Sigma = []).

grdu(A1,restatom(A2,Restr),restsubst(MGU,[])) :-
	member(Theta,Restr),
	applySubst(A2,Theta,A2Theta),
	mgu(A1,A2Theta,MGU).



if_then_else(P,Q,_) :- P,!,Q.
if_then_else(_,_,R) :- R.


