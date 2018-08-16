% substitute(Old,New,OldTerm,NewTerm)
% substitutes all occurrences of Old in OldTerm with
% New, yielding NewTerm
% works only on ground terms

:- module(substitute,[substitute/4]).


substitute(Old,New,Old,New) :- !.
substitute(Old, _ ,Term,Term)	:-	atomic(Term),
					Term \== Old.
substitute(Old,New,Term,Term1)	:-	compound(Term),
					functor(Term,F,N),
					functor(Term1,F,N),
					substitute(N,Old,New,Term,Term1).
substitute(N,Old,New,Term,Term1):-	N>0,
					arg(N,Term,Arg),
					substitute(Old,New,Arg,Arg1),
					arg(N,Term1,Arg1),
					N1 is N-1,
					substitute(N1,Old,New,Term,Term1).
substitute(0,_,_,_,_).

					
