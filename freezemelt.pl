% predicates used to treat non-ground terms as objects
% terms containing get "frozen" to a ground term and can be
% "melted" to put the variables in again

:- module(freezemelt,[freeze/2,melt_new/2]).


freeze(A,B)		:-	copy_term(A,B),
				numbervars(B,v,0,_).



search(Key,node(Key,X,_Left,_Right),Value)	:-
	!,X=Value.
search(Key,node(Key1,_X,Left,_Right),Value)	:-
	Key < Key1,
	search(Key,Left,Value).
search(Key,node(Key1,_X,_Left,Right),Value)	:-
	Key > Key1,
	search(Key,Right,Value).


melt_new(A,B)			:-	melt(A,B,_Dict).

melt(v(N),X,Dict)	        :-	search(N,Dict,X).

melt(X,X,_Dict)			:-	atomic(X).
melt(X,Y,Dict)			:-	compound(X),
					functor(X,F,N),
					functor(Y,F,N),
					melt(N,X,Y,Dict).
melt(N,X,Y,Dict)		:-	N>0,
					arg(N,X,ArgX),
					melt(ArgX,ArgY,Dict),
					arg(N,Y,ArgY),
					N1 is N-1,
					melt(N1,X,Y,Dict).
melt(0,_X,_Y,_Dict).




