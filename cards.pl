:- module(cards, [getAllSuspects/1, getAllRooms/1, getAllWeapons/1,
                  initCards, initSuspects/1, initWeapons/1, suspect/1, room/1, weapon/1,
                  getRemainingRooms/1, getRemainingSuspects/1, getRemainingWeapons/1,
                  removeSuspect/1, removeRoom/1, removeWeapon/1, removeCard/1,
				  removeAllSuspects/1, removeAllWeapons/1, removeAllRooms/1]).

:- dynamic suspect/1.
:- dynamic room/1.
:- dynamic weapon/1.

getAllSuspects(Suspects) :-
  Suspects = ['Mrs Scarlet', 'Colonel Mustard', 'Mrs White', 'Mr Green', 'Mrs Peacock', 'Professor Plum'].
getAllRooms(Rooms) :-
  Rooms = ['Ballroom', 'Conservatory', 'Billiard Room', 'Library', 'Study', 'Hall', 'Lounge', 'Dining Room', 'Kitchen'].
getAllWeapons(Weapons) :-
  Weapons = ['Knife', 'Candlestick', 'Revolver', 'Rope', 'Lead Pipe', 'Wrench'].

getRemainingSuspects(Suspects) :-
  findall(S, suspect(S), Suspects).

getRemainingRooms(Rooms) :-
  findall(R, room(R), Rooms).

getRemainingWeapons(Weapons) :-
  findall(W, weapon(W), Weapons).

removeAllSuspects([]).
removeAllSuspects([Suspect|Suspects]) :-
	removeSuspect(Suspect), removeAllSuspects(Suspects).
	
removeSuspect(Suspect) :-
  retract(suspect(Suspect)).

removeAllRooms([]).
removeAllRooms([Room|Rooms]) :-
	removeRoom(Room), removeAllRooms(Rooms).
removeRoom(Room) :-
  retract(room(Room)).

removeAllWeapons([]).
removeAllWeapons([Weapon|Weapons]) :-
	removeWeapon(Weapon), removeAllWeapons(Weapons).
removeWeapon(Weapon) :-
  retract(weapon(Weapon)).

removeCard(Card) :- suspect(Card), removeSuspect(Card).
removeCard(Card) :- room(Card), removeRoom(Card).
removeCard(Card) :- weapon(Card), removeWeapon(Card).

initCards :-
  getAllSuspects(Suspects),
  getAllRooms(Rooms),
  getAllWeapons(Weapons),
  initSuspects(Suspects),
  initRooms(Rooms),
  initWeapons(Weapons).

initSuspects([]).
initSuspects([Suspect|Suspects]) :-
  assert(suspect(Suspect)),
  initSuspects(Suspects).

initRooms([]).
initRooms([Room|Rooms]) :-
  assert(room(Room)),
  initRooms(Rooms).

initWeapons([]).
initWeapons([Weapon|Weapons]) :-
  assert(weapon(Weapon)),
  initWeapons(Weapons).
