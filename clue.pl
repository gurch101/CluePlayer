:- use_module(helpers).
:- use_module(cards).
:- use_module(menu).
:- use_module(deductions).

% starting point
clue :- init, runGame.

init :-
  initCards,
  initPlayers(Players, HomePlayer, AwayPlayers),
  createAwayPlayers(AwayPlayers),
  createHomePlayer(HomePlayer),
  removeUserCards.

% Ask the user for all the players playing this game, and which player he/she will
% be playing.
initPlayers(Players, HomePlayer, AwayPlayers) :-
  getAllSuspects(Suspects),
  getPlayers(Suspects, TempPlayers, Players),
  findHomePlayer(Players, HomePlayer, AwayPlayers).

getPlayers(Suspects, TempPlayers, Players) :-
  % Allow the user to select suspects until they enter 'b'
  (menu('Select players:', Suspects, Player), (
    delete(Suspects, Player, NewSuspects),
    append(TempPlayers, [Player], NewPlayers), 
    getPlayers(NewSuspects, NewPlayers, Players)
  ));
  Players = TempPlayers.

findHomePlayer(Players, HomePlayer, AwayPlayers) :-
  menu('Select the player you will be playing:', Players, HomePlayer),
  delete(Players, HomePlayer, AwayPlayers).

% used to remove our cards from possible solutions
removeUserCards :- removeMySuspectCards, removeMyRoomCards, removeMyWeaponCards.

removeMySuspectCards :- 
  getRemainingSuspects(Suspects),
  (menu('Which suspect cards do you have? (press b to go back)', Suspects, Suspect), (
	initStateAssertions(Suspect),
	removeMySuspectCards
  )); true.

removeMyRoomCards :- 
  getRemainingRooms(Rooms),
  (menu('Which room cards do you have? (press b to go back)', Rooms, Room), (
    initStateAssertions(Room),
	removeMyRoomCards
  )); true.

removeMyWeaponCards :- 
  getRemainingWeapons(Weapons),
  (menu('Which weapon cards do you have? (press b to go back)', Weapons, Weapon), (
    initStateAssertions(Weapon),
    removeMyWeaponCards
  )); true.

%sequences through each player turn-by-turn
runGame :- 
  mainMenu.
/*
  getAllPlayers(Players), 
  nth1(1, Players, FirstPlayer),
  clueTurn(FirstPlayer).
*/

mainMenu :-
  getMenuOptions(MenuOptions),
  menu('Select an option:', MenuOptions, Selection),
  (menuOption(Selection),
  mainMenu).

getMenuOptions(MenuOptions) :-
  MenuOptions = ['Make a suggestion', 'Record another player\'s suggestion', 'Exit'].

menuOption('Make a suggestion') :-
  getHomePlayer(HomePlayer),
  homeTurn(HomePlayer).

menuOption('Record another player\'s suggestion') :-
  getAwayPlayers(AwayPlayers),
  menu('Who made the suggestion?', AwayPlayers, Player),
  awayTurn(Player).

menuOption('Exit') :- false.

% deal with the home player's turn
clueTurn(CurrentPlayer) :-
  player(CurrentPlayer, home),
  homeTurn(CurrentPlayer),
  getNextPlayer(CurrentPlayer, NextPlayer),
  clueTurn(NextPlayer).

% away players turn
clueTurn(CurrentPlayer) :-
  player(CurrentPlayer, away),
  awayTurn(CurrentPlayer),
  makeDeductions,
  getNextPlayer(CurrentPlayer, NextPlayer),
  clueTurn(NextPlayer).

getNextPlayer(CurrentPlayer, NextPlayer) :-
  getAllPlayers(Players),
  nth1(N, Players, CurrentPlayer),
  Next is N + 1,
  (
    nth1(Next, Players, NextPlayer);
    nth1(1, Players, NextPlayer)
  ).

% write out the solution if there is only 1 combo of cards remaining
homeTurn(CurrentPlayer) :-
  solution,
  write('Solution found! Ask for:'), nl,
  printSuspects,
  printRooms,
  printWeapons.

% Take care of our turn

homeTurn(CurrentPlayer) :-
  write('Its our turn'), nl,
  homeMenu(CurrentPlayer),
  makeDeductions,
  checkForSolution.

homeMenu(CurrentPlayer) :-
  getRemainingSuspects(Suspects),
  getRemainingRooms(Rooms),
  getRemainingWeapons(Weapons),
  getAwayPlayers(AwayPlayers),
  append(AwayPlayers, ['Nobody'], Players),
  menu('Which suspect are you asking about?', Suspects, Suspect),
  menu('Which room are you asking about?', Rooms, Room),
  menu('Which weapon are you asking about?', Weapons, Weapon),
  menu('Who Answered?', Players, AnsBy),
  assert(answer(CurrentPlayer, AnsBy, Suspect, Room, Weapon)),
  retractShownCard(AnsBy, Suspect, Room, Weapon, Shown),
  addHomeStateAssertions(CurrentPlayer, AnsBy, Suspect, Room, Weapon, Shown).

retractShownCard(AnsBy, _, _, _) :-
  AnsBy == 'Nobody'.
  
retractShownCard(AnsBy, SElem, RElem, WElem, Shown) :-
  ((AnsBy \= 'Nobody',
  menu('What type of card did they show you?', ['Suspect', 'Room', 'Weapon'], SCType),
  ((SCType == 'Suspect',
      retract(suspect(SElem)), Shown = SElem);
   (SCType == 'Room',
      retract(room(RElem)), Shown = RElem);
   (SCType == 'Weapon',
      retract(weapon(WElem))), Shown = WElem))).

awayTurn(CurrentPlayer) :-
  getAllSuspects(Suspects),
  getAllRooms(Rooms),
  getAllWeapons(Weapons),
  getAllPlayers(AllPlayers),
  append(AllPlayers, ['Nobody'], AllPlayers2),
  delete(AllPlayers2, CurrentPlayer, Players),
  string_concat(CurrentPlayer, ' ask about?', MenuStringTail),
  string_concat('Which suspect did ', MenuStringTail, SuspectMenuString),
  string_concat('Which room did ', MenuStringTail, RoomMenuString),
  string_concat('Which weapon did ', MenuStringTail, WeaponMenuString),
  menu(SuspectMenuString, Suspects, Suspect),
  menu(RoomMenuString, Rooms, Room),
  menu(WeaponMenuString, Weapons, Weapon),
  menu('Which player answered?', Players, AnsBy),
  assert(question(CurrentPlayer, Suspect)),
  assert(question(CurrentPlayer, Room)),
  assert(question(CurrentPlayer, Weapon)),
  assert(answer(CurrentPlayer, AnsBy, Suspect, Room, Weapon)),
  addAwayStateAssertions(CurrentPlayer, AnsBy, Suspect, Room, Weapon).
      
getAwaySuspect(SElem) :- getAllSuspects(Suspects), printList(Suspects), read(N), nth1(N,Suspects,SElem).
getAwayRoom(RElem) :- getAllRooms(Rooms), printList(Rooms), read(N), nth1(N,Rooms,RElem).
getAwayWeapon(WElem) :- getAllWeapons(Weapons), printList(Weapons), read(N), nth1(N,Weapons,WElem).

createAwayPlayers([]).
createAwayPlayers([AwayPlayer|AwayPlayers]) :-
  assert(player(AwayPlayer, away)),
  createAwayPlayers(AwayPlayers).

createHomePlayer(HomePlayer) :-
  assert(player(HomePlayer, home)).

getHomePlayer(HomePlayer) :- 
  player(HomePlayer, home).
getAwayPlayers(OrderedPlayers) :- 
  findall(X, player(X, away), AwayPlayers),
  reorderPlayers(AwayPlayers, OrderedPlayers).
getAllPlayers(OrderedPlayers) :- 
  findall(X, player(X, Y), Players),
  reorderPlayers(Players, OrderedPlayers).

reorderPlayers(Players, OrderedPlayers) :-
  getAllSuspects(Suspects),
  reorderPlayers1(Suspects, Players, [], OrderedPlayers).

reorderPlayers1(Suspects, [], TempPlayers, OrderedPlayers) :-
  OrderedPlayers = TempPlayers.
reorderPlayers1([Suspect|Suspects], Players, TempPlayers, OrderedPlayers) :-
  member(Suspect, Players),
  delete(Players, Suspect, NewPlayers),
  append(TempPlayers, [Suspect], NewTempPlayers),
  reorderPlayers1(Suspects, NewPlayers, NewTempPlayers, OrderedPlayers).
reorderPlayers1([Suspect|Suspects], Players, TempPlayers, OrderedPlayers) :-
  not(member(Suspect, Players)),
  reorderPlayers1(Suspects, Players, TempPlayers, OrderedPlayers).

getSuspect(NS, Elem) :- findall(Y, suspect(Y),L), nth1(N,L, Elem).

lastSuspect :- findall(Y, suspect(Y), L), length(L, 1).
lastWeapon :- findall(Y, weapon(Y), L), length(L, 1).
lastRoom :- findall(Y, room(Y), L), length(L, 1).

solution :- lastSuspect, lastWeapon, lastRoom.
checkForSolution :- solution,
  write('Solution found! Ask for:'), nl,
  printSuspects,
  printRooms,
  printWeapons.