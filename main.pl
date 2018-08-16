% Rational Model Generator - Main Program
% load(+Normal Logic Program File)
% iter(+NumberOfIterations)


:- module(main,[iter_init/0,iter/0,iter/1,load/1]).

:- use_module(tptilde).
:- use_module(substitute).
:- use_module(subterm). 
:- use_module(readnlp).
:- use_module(rename).



load(X) :- 
	retractall(readnlp:nlpFact(_)),
	retractall(readnlp:nlpNeg(_,_)),
	retractall(readnlp:nlpPos1(_,_)),
	retractall(readnlp:nlpPos2(_,_,_)),
	readnlp(X).


iter_init :- retractall(currentInt(_)),
	     asserta(currentInt([])).

iter :- currentInt(CurrentInt),
	retract(currentInt(_)),
	initialize_renaming,
	tptilde(CurrentInt,NewInt),
	asserta(currentInt(NewInt)),
	pretty_print(NewInt).


iter(N) :- iter(N,[],I),
	   pretty_print(I).


iter(N,Acc,I) :- N > 0,
	           initialize_renaming,
                   tptilde(Acc,NewInt),
                   N1 is N - 1,
		   iter(N1,NewInt,I).
iter(_N,I,I).

             
pretty_print([]).
pretty_print([H|T]) :- 
	unfailing_setof(X,subterm(v(X),H),Vars),
	H = restatom(H1,_), % don't print restriction
	replaceall(Vars,H1,Hpretty,0),
	write_ln(Hpretty),
	pretty_print(T).

replaceall([],H,H,_N).
replaceall([N|T],H,Hnew,Count) :- 
	Count1 is Count+1,
	concat(x,Count1,Name),
        substitute(v(N),Name,H,H1),
        replaceall(T,H1,Hnew,Count1).

	                      


unfailing_setof(X,P,S) :- setof(X,P,S), !.
unfailing_setof(_,_,[]).





