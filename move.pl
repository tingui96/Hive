:-consult(list),import(list).
:-consult(hive), import(hive).

:- module(game,
    [player/6,
    current_player/1,
    init_game/2,
    reset_game/1, 
    new_game/3, 
    next_player/0,
    increment_number_of_moves/1,
    place_queen_bee/1
    ]).

:-dynamic player/6, current_player/1, mode/1, level/1.

% next player
next_player():-
    current_player(P),
    P == p1,
    retract(current_player(p1)),
    assert(current_player(p2)),!.
next_player():-
    current_player(P),
    P == p2,
    retract(current_player(p2)),
    assert(current_player(p1)).



queen_bee_move([],_,_,_):-!.
queen_bee_move([U,_][Q])

