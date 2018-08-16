% Rational Tp Operator
% tptilde(+CurrentInterpretation,-NewInterpretation)

:- module(tptilde,[tptilde/2]).

:- use_module(mgru).
:- use_module(grdu).
:- use_module(glb).
:- use_module(applyRestSubst).
:- use_module(rename).
:- use_module(subterm).

tptilde(CurrentInt,NewIntRenamed) :- 
	unfailing_setof(R,putIntoNewInt(CurrentInt,R),NewInt),
	maplist(rename,NewInt,NewIntRenamed).


putIntoNewInt(CurrentInt,Rnorm) :- 
	isInNewInt(CurrentInt,R),
	normalize(R,Rnorm).


isInNewInt(_CurentInt,restatom(A,[])) :- readnlp:nlpFact(A).

isInNewInt(CurrentInt,ATheta) :- 
	readnlp:nlpNeg(A,A1),
	fitsNegative(CurrentInt,A1,Theta),
	applyRestSubst(A,Theta,ATheta).

isInNewInt(CurrentInt,ATheta) :- 
	readnlp:nlpPos1(A,A1),
        fitsPositive(CurrentInt,A1,Theta),
	applyRestSubst(A,Theta,ATheta).

isInNewInt(CurrentInt,ATheta) :- 
	readnlp:nlpPos2(A,A1,A2),
        fitsPositive(CurrentInt,A1,Theta1),
	fitsPositive(CurrentInt,A2,Theta2),
	constrPred([A,A1,A2],B),
	glb3(B,Theta1,Theta2,Theta),
	applyRestSubst(A,Theta,ATheta).



fitsPositive(CurrentInt,A,Theta) :- member(R,CurrentInt),
	                            mgru(restatom(A,[]),R,Theta).

fitsNegative(CurrentInt,A,Theta) :- oneGrduPerR(CurrentInt,A,GrduList,[]),
                                    glb(A,GrduList,Theta).

oneGrduPerR([],_A,GrduList,GrduList).
oneGrduPerR([H|T],A,GrduList,Acc) :- grdu(A,H,GRDU),
	                             oneGrduPerR(T,A,GrduList,[GRDU|Acc]).

constrPred(L,P) :- unfailing_setof(v(X),subterm(v(X),L),P).

unfailing_setof(X,P,S) :- setof(X,P,S), !.
unfailing_setof(_,_,[]).
































