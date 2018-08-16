% Variable renaming
% rename(+TermToRename,+ToBeFreeFor,-RenamedTerm)
% renameInt(+Interpretation,-RenamedInterpretation)

:- module(rename,[normalize/2,rename/2,initialize_renaming/0]).

:- use_module(substitute).
:- use_module(subterm).

:- dynamic renamingNumber/1.

normalize(Term,TermNormalized) :- 
	unfailing_setof(X,subterm(v(X),Term),Vars),
	normalize_aux(Vars,Term,TermNormalized,0).

normalize_aux([],H,H,_N).
normalize_aux([N|T],H,Hnew,Count) :- 
	Count1 is Count+1,
        substitute(v(N),v(Count),H,H1),
        normalize_aux(T,H1,Hnew,Count1).        
	             

initialize_renaming :- 
	if_then_else(renamingNumber(RNum),
		      retract(renamingNumber(RNum)),
		      true),
        determine_initial_renamingNumber(IN),
        asserta(renamingNumber(IN)).

% determines the first variable that is not used in the program
determine_initial_renamingNumber(IN) :-
	unfailing_setof(X,readnlp:nlpFact(X),Facts),
	unfailing_setof([X1,X2],readnlp:nlpNeg(X1,X2),Negs),
	unfailing_setof([X3,X4],readnlp:nlpPos1(X3,X4),Pos1),
	unfailing_setof([X5,X6,X7],readnlp:nlpPos2(X5,X6,X7),Pos2),
	All = just_a_term(Facts,Negs,Pos1,Pos2),
	unfailing_setof(N,subterm(v(N),All),Vars),
	maximum(Vars,MAX),
	IN is MAX +1.

maximum([N],N).
maximum([N|T],M) :- maximum(T,TM), M is max(N,TM).


rename(T,Tren) :-
	unfailing_setof(X,subterm(v(X),T),Vars),
	length(Vars,NumberOfVars),
	renamingNumber(RNum),
        NewRNum is RNum + NumberOfVars,
	retract(renamingNumber(RNum)),
	asserta(renamingNumber(NewRNum)),
	replaceall(Vars,T,Tren1,RNum),
	restore_v(NewRNum,RNum,Tren1,Tren),
	!.

restore_v(Count,RNum,T,Trestored) :-
	Count > RNum,
	Count1 is Count - 1,
	substitute(vreplaced(Count1),v(Count1),T,Tnew),
	restore_v(Count1,RNum,Tnew,Trestored).
restore_v(_,_,T,T) :-!.


replaceall([],H,H,_N).
replaceall([N|T],H,Hnew,Count) :- 
	Count1 is Count+1,
        substitute(v(N),vreplaced(Count),H,H1),
        replaceall(T,H1,Hnew,Count1).        
	             






/* old stuff ..
renameInt(Int,RenInt) :- ren_aux(Int,RenInt,[]).
ren_aux([],[],_).
ren_aux([H|T],[Hrenamed|Trenamed],AlreadyRenamed) :- 
	rename(H,[T,AlreadyRenamed],Hrenamed),
	ren_aux(T,Trenamed,[Hrenamed|AlreadyRenamed]).



rename(Term1,Term2,NewTerm1) :-
	unfailing_setof(X,subterm(v(X),Term1),Vars1),
	unfailing_setof(X,subterm(v(X),Term2),Vars2),
	intersection(Vars1,Vars2,I),
	union(Vars1,Vars2,U),
	replaceInTerm(I,Term1,NewTerm1,U).


replaceInTerm([],NewTerm,NewTerm,_Acc).
replaceInTerm([H|T],Term,NewTerm,Acc) :- 
	leastNumberNotInSet(N,Acc),
	substitute(v(H),v(N),Term,TermSubst),
	replaceInTerm(T,TermSubst,NewTerm,[N|Acc]).


*/

unfailing_setof(X,P,S) :- setof(X,P,S), !.
unfailing_setof(_,_,[]).

if_then_else(P,Q,_) :- P,!,Q.
if_then_else(_,_,R) :- R.

















