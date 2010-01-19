:- module(helpers, [printSuspects, printRooms, printWeapons,
                    printList/1, printListNoNum/1, trim/3, deleteIndex/3,
					getPlayersBetween/3]).

% print functions to print facts
printSuspects :- findall(Y, suspect(Y), L), printList(L).
printRooms :- findall(Y, room(Y), L), printList(L).
printWeapons :- findall(Y, weapon(Y), L), printList(L).

% prints each item and item index of a list on a new line. Starts from 1.
printList(L) :- printList1(L, 1).
printList1([], N).
printList1([X|Xs], N) :- write(N), write('. '), write(X), nl, Y is N + 1, printList1(Xs, Y). 

printListNoNum([]).
printListNoNum([X|Xs]) :- write(X), nl, printListNoNum(Xs).

% trims a list to a specified length
trim(L, Len, NewL) :- 
  nth0(Len, L, Elem),
  append(NewL, [Elem|X], L).

deleteIndex(Index, List, NewList) :-
  nth1(Index, List, Item),
  delete(List, Item, NewList).

% retrieves the players who fall between the person who asked the question
% and the person who answered.
getPlayersBetween(QBy, ABy, L) :-
   getAllPlayers(Players),
   nth0(QPos, Players, QBy),
   nth0(APos, Players, ABy),
   ((QPos < APos, getPlayersBetweenNorm(Players, QPos, APos, L));
   (QPos > APos, length(Players, Num), getPlayersBetweenWrap(Players, QPos, APos, L, Num))).

getPlayersBetweenNorm(Players, QPos, APos, []) :- Z is APos - 1, QPos == Z.
getPlayersBetweenNorm(Players, QPos, APos, [X|Xs]) :-
   Btwn is QPos + 1, Btwn < APos, nth0(Btwn, Players, P),  X = P, getPlayersBetweenNorm(Players, Btwn, APos, Xs).

getPlayersBetweenWrap(Players, QPos, APos, Xs, Num) :- Z is Num - 1, QPos == Z, trim(Players, APos, Xs).
getPlayersBetweenWrap(Players, QPos, APos, [X|Xs], Num) :-
   Btwn is QPos + 1, Btwn < Num, nth0(Btwn, Players, P), X = P, getPlayersBetweenWrap(Players, Btwn, APos, Xs, Num). 


	