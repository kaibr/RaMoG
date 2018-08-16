% subterm(Subterm,+Term)
% succeeds if Subterm is a subterm of Term
% works only on ground terms

:- module(subterm,[subterm/2]).


subterm(Term,Term).
subterm(Subterm,Term)	:-	compound(Term),
				functor(Term,_F,N),
				subterm(N,Subterm,Term).


subterm(N,Subterm,Term)	:-	N>1,
				N1 is N-1,
				subterm(N1,Subterm,Term).
subterm(N,Subterm,Term)	:-	arg(N,Term,Arg),
				subterm(Subterm,Arg).

			
