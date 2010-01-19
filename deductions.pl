:- module(deductions, [initStateAssertions/1, addHomeStateAssertions/6, 
					   addAwayStateAssertions/5, makeDeductions,
					   deduceDontHaves, checkDontHave/5,
					   addAwayDontHaves/1, addAwayDontHavesExcludeOne/2,
					   checkAndAssertPlayerHas/2, addAwayDontHavesToPlayers/2]).
:- use_module(cards).
:- use_module(helpers).

:- dynamic dontHave/2.
:- dynamic playerHas/2.
dontHave('a','a').
playerHas('a', 'a').

% removes home players cards from possible set of suspects, weapons, and rooms
% adds dontHave(Player, Item) for all away players
initStateAssertions(Item) :-
	addAwayDontHaves(Item),
	getHomePlayer(HomePlayer),
	checkAndAssertPlayerHas(HomePlayer, Item),
	removeCard(Item).

% adds the necessary dontHave(Player, Item)
% this includes adding dontHave's for Suspect, Room, Weapon for all players
% between the home player and the player that answered
% and adding dontHave's for the shown card all away players except the player that showed us the card.
addHomeStateAssertions(QBy, AnsBy, Suspect, Room, Weapon, Shown) :-
	% no one answered and we dont have one or more of the cards we asked about
	((AnsBy == 'Nobody',
	 determineSuspect(QBy, Suspect),
	 determineWeapon(QBy, Weapon),
	 determineRoom(QBy, Room),
	 addListAwayDontHaves([Suspect,Room,Weapon]));
	(AnsBy \= 'Nobody',
	 checkAndAssertPlayerHas(AnsBy, Shown),
	 getPlayersBetween(QBy, AnsBy, Players),
	 addAwayDontHavesToPlayers(Suspect, Players),
	 addAwayDontHavesToPlayers(Room, Players),
	 addAwayDontHavesToPlayers(Weapon, Players),
	 addAwayDontHavesExcludeOne(Shown,AnsBy))).

addAwayStateAssertions(QBy, AnsBy, Suspect, Room, Weapon) :-
	% no one answered therefore we know everyone except maybe the question asker has none of the cards
	((AnsBy == 'Nobody',
	 addAwayDontHavesExcludeOne(Suspect,QBy),
	 addAwayDontHavesExcludeOne(Room,QBy),
	 addAwayDontHavesExcludeOne(Weapon,QBy));
	% someone answered so we know everyone between the question asker and answerer dont have the cards
	(AnsBy \= 'Nobody',
	 getPlayersBetween(QBy, AnsBy, Players),
	 addAwayDontHavesToPlayers(Suspect, Players),
	 addAwayDontHavesToPlayers(Room, Players),
	 addAwayDontHavesToPlayers(Weapon, Players))).

checkAndAssertPlayerHas(Player, Item) :- playerHas(Player,Item).
checkAndAssertPlayerHas(Player, Item) :- not(playerHas(Player,Item)), assert(playerHas(Player,Item)).

% add a list of items that no away player has
addListAwayDontHaves([]).
addListAwayDontHaves([Item|Items]) :- addAwayDontHaves(Item), addListAwayDontHaves(Items).

% add an item that no away player has	
addAwayDontHaves(Item) :- getAwayPlayers(Players), addAwayDontHavesToPlayers(Item,Players).

% add an item that no players in the list has
addAwayDontHavesToPlayers(Item, []).
addAwayDontHavesToPlayers(Item,[Player|Players]) :- player(Player, away), not(dontHave(Player,Item)), assert(dontHave(Player,Item)), addAwayDontHavesToPlayers(Item,Players).
addAwayDontHavesToPlayers(Item, [Player|Players]) :- (player(Player, home);dontHave(Player,Item)), addAwayDontHavesToPlayers(Item, Players).

% add an item that no away player except ExcPlayer has
% when its our turn and a player shows us a card, 
% we add dontHave(Player, Item), for every player
% except the one that showed us a card.
addAwayDontHavesExcludeOne(Item, ExcPlayer) :-
	getAwayPlayers(P), delete(P, ExcPlayer, L), addAwayDontHavesToPlayers(Item, L).

% if no one answered our question and we dont have the 
% suspect in question, we know the suspect
determineSuspect(HomePlayer, Suspect) :-
	((not(playerHas(HomePlayer, Suspect)),
	getRemainingSuspects(Suspects), 
	delete(Suspects, Suspect, L),
	removeAllSuspects(L));true).

% if no one answered our question and we dont have the 
% weapon in question, we know the weapon
determineWeapon(HomePlayer, Weapon) :-
	((not(playerHas(HomePlayer, Weapon)),
	getRemainingWeapons(Weapons), 
	delete(Weapons, Weapon, L),
	removeAllWeapons(L));true).

% if no one answered our question and we dont have the 
% room in question, we know the room
determineRoom(HomePlayer, Room) :-
	((not(playerHas(HomePlayer, Room)),
	getRemainingRooms(Rooms), 
	delete(Rooms, Room, L),
	removeAllRooms(L));true).

makeDeductions :- deduceDontHaves, makeDeductions.
makeDeductions.

deduceDontHaves :- answer(QBy, ABy, Suspect, Weapon, Room), 
				checkDontHave(ABy, Suspect, Weapon, Room, ToRemove), removeCard(ToRemove),
				assert(playerHas(ABy, ToRemove)),
				addAwayDontHavesExcludeOne(ToRemove, ABy).  

checkDontHave(Player, Suspect, Weapon, Room, Suspect) :- 
	dontHave(Player, Weapon), dontHave(Player, Room), not(playerHas(Player, Suspect)).

checkDontHave(Player, Suspect, Weapon, Room, Weapon) :-
	dontHave(Player, Suspect), dontHave(Player, Room), not(playerHas(Player, Weapon)).

checkDontHave(Player, Suspect, Weapon, Room, Room) :-
	dontHave(Player, Suspect), dontHave(Player, Weapon), not(playerHas(Player, Room)).
	