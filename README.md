
Rational Model Generator

Implementation in Prolog of a Tp operator for logic programs with negation. 

Generates "rational models". https://link.springer.com/chapter/10.1007/3-540-61708-6_40

It's a reliable method for producing stack overflows.

load('<Filename>'). 
	loads the normal logic program from file <Filename>

iter(N).
	iterates Tp N times and outputs the resulting interpretation


iter_init.
	initializes stepwise iteration of Tp

iter.
	applies Tp to the current interpretation and outputs
	the resulting interpretation


NotYetImplemented:

query(<Query>).
	iterates Tp starting from the empty set, printing a message
	at every iterative step, stopping when there is an atom
	in the current interpretation that unifies with <Query>