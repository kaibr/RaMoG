% Greatest Lower Bound
% glb(+ListofRestrSubsts,-GLB)

:- module(glb,[glb/3,glb3/4]).

:- use_module(mgru).
:- use_module(applyRestSubst).
:- use_module(applySubst).
:- use_module(subterm).
:- use_module(rename).


glb(P,L,GLB) :- glb_aux(L,P,[],GLB).

glb_aux([GLB1],P,Sig,GLB)   :- 
	applySubst(P,Sig,PSig),
	applyRestSubst(PSig,GLB1,PSigGLB1),
	mgru(restatom(P,[]),PSigGLB1,GLB).

glb_aux([H1,H2|T],P,Sig,GLB) :-
        applySubst(P,Sig,PSig),
	applyRestSubst(PSig,H1,PH1),
	applyRestSubst(P,H2,PH2),
	mgru(PH1,PH2,MGRU),
        H2 = restsubst(Subst,_Rest),
	glb_aux([MGRU|T],P,Subst,GLB).


glb3(B,Theta1,Theta2,Theta) :-
	applyRestSubst(B,Theta1,BTheta1),
	applyRestSubst(B,Theta2,BTheta2),
	rename(BTheta1,BT1ren),
	rename(BTheta2,BT2ren),
	mgru(BT1ren,BT2ren,MGRU),
	BT1ren = restatom(A,_),
	applyRestSubst(A,MGRU,AMGRU),
	mgru(restatom(B,[]),AMGRU,Theta).






