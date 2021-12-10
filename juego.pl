:-consult(list),import(list).
:-consult(hive), import(hive).

:- module(game,
    [player/5,
    current_player/1,
    init_game/2,
    reset_game/1, 
    new_game/3, 
    next_player/0,
    increment_number_of_moves/1,
    place_queen_bee/1
    ]).

:-dynamic player/5, current_player/1, modo/1, dificultad/1.




