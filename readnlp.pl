% readnlp(+Filename)
% reads the Normal Logic Program and asserts nlpClauses


:- module(readnlp,[readnlp/1]).

:- use_module(freezemelt).
:- use_module(subterm).

:- dynamic nlpFact/1,nlpNeg/2,nlpPos1/2,nlpPos2/3.


readnlp(File) :-
	seeing(Input),
	see(File),
	repeat,
	read(Term),
	process(Term),
	setof(C,nlpClause(C),Clauses),
	retractall(nlpClause(_)),
	convertToNF(Clauses,ClausesNF),
	assertnlp(ClausesNF),
       	seen,
	see(Input),
	!.

process(end_of_file) :- !.
process(Term) :- freeze(Term,Frozen),
	         asserta(nlpClause(Frozen)),
		 fail.


isNF(C) :- isFact(C);
	   isNeg(C);
	   isPos1(C);
	   isPos2(C).

isFact(C) :- C \= (_ :- _).
isNeg(C) :- C = (_ :- (not _)).
isPos1(C) :- C = (_ :- B),
	      B \= (not _),
	      B \= (_,_).
isPos2(C) :- C = (_ :- (A,Rest)),
	      A \= (not _),
	      Rest \= (not _),
	      Rest \= (_,_).


convertToNF(Clauses,ClausesNF) :-
	member(C,Clauses),
	not isNF(C),
	C = (_ :- (A,_)), 
	delete(Clauses,C,RestClauses),
	if_then_else( A = (not _),
	              convNeg(C,RestClauses,NewClauses),
		      convPos(C,RestClauses,NewClauses)),
        !,
        convertToNF(NewClauses,ClausesNF).

convertToNF(ClausesNF,ClausesNF).

convPos(H:-(A,Rest),RestClauses,NewClauses) :-
	unfailing_setof(v(X),subterm(v(X),Rest),Args),
	gensym(aux,AuxPred),
	AuxHead =.. [AuxPred|Args],
	NewC1 = [H:- (A,AuxHead)|RestClauses],
	NewClauses = [AuxHead :- Rest|NewC1].

convNeg(H:-((not A),Rest),RestClauses,NewClauses) :-
	A =.. [P|Args],
	gensym(P,Pnew),
	Anew =.. [Pnew|Args],
	NewC1 = [H:-(Anew,Rest)|RestClauses],
	NewClauses = [Anew :- not A|NewC1].

assertnlp([]).
assertnlp([H|T]) :- isFact(H),
	            asserta(nlpFact(H)),
		    assertnlp(T).

assertnlp([H :- not A|T]) :- isNeg(H :- not A),
	                     asserta(nlpNeg(H,A)),
		             assertnlp(T).

assertnlp([H :- A|T]) :- isPos1(H:-A),
	                 asserta(nlpPos1(H,A)),
		         assertnlp(T).

assertnlp([H :- (A,B)|T]) :- isPos2(H:-(A,B)),
	                     asserta(nlpPos2(H,A,B)),
		             assertnlp(T).

if_then_else(P,Q,_) :- P,!,Q.
if_then_else(_,_,R) :- R.

unfailing_setof(X,P,S) :- setof(X,P,S), !.
unfailing_setof(_,_,[]).

