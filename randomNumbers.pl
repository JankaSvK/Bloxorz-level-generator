% PROLOG
generateListOfRanNum(0, []) :- !.
generateListOfRanNum(N,[X|List]) :-
	random_between(1,2,X), %len na dva smery
	!,
	succ(NewN, N),
	generateListOfRanNum(NewN, List)
.

createMap(0, _, []) :- !.
createMap(Width, Height, [X|M1]) :-
	length([X|M1], Width),
	length(X, Height),
	succ(NewWidth,Width),
	createMap(NewWidth, Height, M1)
.

setVisited(X, Y, IM) :-
	nth1(Y,IM,Column),
	nth1(X,Column,t)
.

setVisited(X, Y, 0, IM) :-
	setVisited(X, Y, IM)
.

setVisited(X, Y, 1, IM) :-
	setVisited(X, Y, IM),
	succ(X, NX),
	setVisited(NX, Y, IM)
.

setVisited(X, Y, 2, IM) :-
	setVisited(X, Y, IM),
	succ(Y, NewY),
	setVisited(X, NewY, IM)
.

%newCoords(X, Y, Stav, Posun, NX, NY) ;-
newStateAndCoords(X, Y, 0, 1, NX, Y, 1) :-
	succ(X, NX)
.

newStateAndCoords(X, Y, 0, 2, X, NY, 2) :-
	succ(Y, NY)
.

newStateAndCoords(X, Y, 1, 1, NX, Y, 0) :-
	succ(X, NNX),
	succ(NNX, NX)
.

newStateAndCoords(X, Y, 1, 2, X, NY, 1) :-
	succ(Y, NY)
.

newStateAndCoords(X, Y, 2, 1, NX, Y, 2) :-
	succ(X, NX)
.

newStateAndCoords(X, Y, 2, 2, X, NY, 0) :-
	succ(Y, NNY),
	succ(NNY, NY)
.

% stavy: 0-stojim, 1-horizontalne, 2-vertikalne
movingWithTheBlock(X, Y, S, IMatrix, []) :-
	setVisited(X, Y, S, IMatrix)
.	
movingWithTheBlock(X, Y, S, IMatrix, [M|Movements]) :-
	setVisited(X, Y, S, IMatrix),
	newStateAndCoords(X, Y, S, M, NX, NY, NS),
	movingWithTheBlock(NX, NY, NS, IMatrix, Movements)
.	
