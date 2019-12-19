% Assignment 6
% Written by Liam Murphy

sched :-
		courses(Courses),
	
		% English is taught in Room 120
		member(course(english,120,_,_), Courses),

		% The history teacher uses an overhead projector.
		member(course(history,_,_,projector), Courses),

		% Ms. Smith teaches in Room 200
		member(course(_,200,smith,_), Courses),

		% The Math class meets in the hour before the class in Room 233
		before(course(math,_,_,_), course(_,233,adams,_), Courses),

		% The course in Room 200 uses models
		member(course(_,200,_,models), Courses),

		% Mr. Whitherspoon uses the blackboard
		member(course(_,_,whitherspoon,blackboard), Courses),

		% Ms. Johnson teaches the hour before Science is taught.
		before(course(_,_,johnson,_), course(science,_,_,_), Courses),

		% Mr. Adams teaches in Room 233
		member(course(_,233,adams,_), Courses),
		
		% A lectern is used in the class at 9:00
		member(course(_,_,_,lecturn), Courses),

		% Room 105 holds a class at 11:00AM
		member(course(_,105,_,_), Courses),

		print_courses(Courses).	

		
% S R T P
courses([
		course(_,_,_,_),
		course(_,_,_,_),
		course(_,_,_,_),
		course(_,_,_,_)
]). 

before(A, B, [A, B | _]).
%before(A, B, [B, A | _]).
before(A, B, [_ | Y]) :- before(A, B, Y).


print_courses([A|B]) :- !,
		write(A), nl,
		print_courses(B).
print_courses([]).


:- initialization(sched).
