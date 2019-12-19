% Crimes for Part 1 of Assignment 6
% Written by Liam Murphy

crimes([charge(norma,_), charge(andy,_), charge(edward,_), charge(olivia,_)]).

solution(List) :- crimes(List),

	member(charge(_,sign),List), member(charge(_,speeding),List), member(charge(_,turn),List), member(charge(_,light),List),

	(member(charge(norma,speeding),List); member(charge(norma,turn),List)),
	(member(charge(andy,sign),List); member(charge(andy,turn),List); member(charge(andy,light),List)),
	(member(charge(olivia,speeding),List); member(charge(olivia,turn),List); member(charge(olivia,light),List)),
	(member(charge(edward,turn),List)).
