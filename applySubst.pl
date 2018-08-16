% applySubst(+Atom,+Substitution,-AtomSubst)


:- module(applySubst,[applySubst/3]).



applySubst(T,L,Tnew) :- substitute(L,T,Tnew).


substitute(L,v(N),T) :- member(subst(v(N),T),L),!.

substitute(L,Term,Term)	:- 
	atomic(Term),
	not member(subst(Term,_),L).

substitute(L,Term,Term1)	:-	compound(Term),
					functor(Term,F,N),
					functor(Term1,F,N),
					substitute(N,L,Term,Term1).
substitute(N,L,Term,Term1):-	        N>0,
					arg(N,Term,Arg),
					substitute(L,Arg,Arg1),
					arg(N,Term1,Arg1),
					N1 is N-1,
					substitute(N1,L,Term,Term1).
substitute(0,_,_,_).

					