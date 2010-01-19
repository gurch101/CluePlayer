:- module(menu, [menu/3, menu/2]).

:- use_module(cards).
:- use_module(helpers).

menu(Message, Options, Selection) :-
  write(Message), nl,
  printList(Options),
  read(Num),
  ((number(Num),
    findSelection(Num, Options, Selection));

  (menuFunction(Num) ->
  menu(Message, Options, Selection))).

menu(Message, Selection) :- menu(Message, [], Selection).
findSelection(N, [], Selection) :- Selection = N.
findSelection(N, Options, Selection) :- nth1(N, Options, Selection).

menuFunction('p') :-
  write('printing info'), nl.
menuFunction('w') :-
  write('printing weapons'), nl.
menuFunction('b') :- false.
