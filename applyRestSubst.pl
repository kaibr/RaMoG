% applyRestSubst(+Atom,+restrictedSubstitution,-restrictedAtom)


:- module(applyRestSubst,[applyRestSubst/3]).

:- use_module(applySubst).

applyRestSubst(Atom,restsubst(Subst,Restr),restatom(AtomSubst,Restr)) :-
	applySubst(Atom,Subst,AtomSubst).








