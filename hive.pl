:- module(hive, 
    [
        piece/6,
        list_piece/7,
        init_piece/0,
        set_piece/4,
        posible_colocacion/3,
        posibles_movimientos/4,
        player/5,
        todos_vecinos_vacios_colmena/1,
        queen_bee_in_box/1
    ]).

:-consult(cell), import(cell).
:-consult(list), import(list).

:-dynamic piece/6, player/5, current_player/1, node/2.
% piece(Type, Id, PlayerId, Hex=[Q,R], Placed, LVL)

other_player(p1, p2).
other_player(p2 ,p1).
% list of pieces with filter
list_piece(Type, Id, Player_id, Hex, Placed, Lvl, Pieces):-
    findall([Type, Id, Player_id, Hex, Placed, Lvl], piece(Type, Id, Player_id, Hex, Placed, Lvl), Pieces).

/* Inicializacion de todas las piezas */
init_piece():-
    assert(piece(queen_bee, 1, p1, none, false,0)),
    assert(piece(queen_bee, 1, p2, none, false,0)),

    assert(piece(beetle, 1, p1, none, false,0)),
    assert(piece(beetle, 2, p1, none, false,0)),
    assert(piece(beetle, 1, p2, none, false,0)),
    assert(piece(beetle, 2, p2, none, false,0)),

    assert(piece(grasshopper, 1, p1, none, false,0)),
    assert(piece(grasshopper, 2, p1, none, false,0)),
    assert(piece(grasshopper, 3, p1, none, false,0)),
    assert(piece(grasshopper, 1, p2, none, false,0)),
    assert(piece(grasshopper, 2, p2, none, false, 0)),
    assert(piece(grasshopper, 3, p2, none, false,0)),

    assert(piece(spider, 1, p1, none, false,0)),
    assert(piece(spider, 2, p1, none, false,0)),
    assert(piece(spider, 1, p2, none, false,0)),
    assert(piece(spider, 2, p2, none, false,0)),

    assert(piece(soldier_ant, 1, p1, none, false,0)),
    assert(piece(soldier_ant, 2, p1, none, false,0)),
    assert(piece(soldier_ant, 3, p1, none, false,0)),
    assert(piece(soldier_ant, 1, p2, none, false,0)),
    assert(piece(soldier_ant, 2, p2, none, false,0)),
    assert(piece(soldier_ant, 3, p2, none, false,0)),

    assert(piece(ladybug, 1, p1, none, false,0)),
    assert(piece(ladybug, 1, p2, none, false,0)),

    assert(piece(mosquito, 1, p1, none, false,0)),
    assert(piece(mosquito, 1, p2, none, false,0)),

    assert(piece(pillbug, 1, p1, none, false,0)),
    assert(piece(pillbug, 1, p2, none, false,0)).

hexagonos_ocupados(Hive_hex):-
    findall(H, piece(_, _, _, H, true,1), Hive_hex).

% colocar pieza
% set_piece(Player_id, Type, Hex, Insect)
set_piece(Player_id, Type, Hex, Piece):-
    var(Piece),
    atom(Player_id),
    atom(Type),
    compound(Hex),
    var(Piece),
    piece(Type, Id, Player_id, none, false, 0),
    !,
    retract(piece(Type, Id, Player_id, none, false, 0)),
    assert(piece(Type, Id, Player_id, Hex, true, 1)),
    piece(Type, Id, Player_id, Hex, true, 1)=..Piece.

%Algoritmo DFS
dfs_visit([[Goal|Path]|_],Goal,[Goal|Path],0,_):-!.
dfs_visit([Path|Queue],Goal,FinalPath,N,Hive) :-
    extend(Path,NewPaths, Hive),
    append(NewPaths,Queue,NewQueue),
    dfs_visit(NewQueue,Goal,FinalPath,M,Hive),
    N is M+1.

%Algoritmo BFS
% bfs (Queue, Visited, Lvl, AdjPred) | Expand the search as long as it does not exceed the Lvl. Uses AdjPred to find adj hexagons
bfs_lvl([], _, _, _):-!.
bfs_lvl([[_, _lvl]|_], _, Lvl, _):-
    _lvl > Lvl,!.
bfs_lvl([[U, _]|Q], Visited, Lvl, AdjPred):-
    member(U, Visited),
    bfs_lvl(Q, Visited, Lvl, AdjPred).
bfs_lvl([[U, _lvl]|Q], Visited, Lvl, AdjPred):-
    not(member(U, Visited)),
    assert(node(U, _lvl)),
    Pred =..[AdjPred, U],
    call(Pred),
    % valid_adj([U, _lvl], A),
    append([U], Visited, Visited1),
    append(Q, A, Q1),
    bfs_lvl(Q1, Visited1, Lvl, AdjPred).

% hace dfs parra saber si esta conectado el hexagono 1 con el 2
is_connected(Hex1,Hex2,Hive):-
    dfs_visit([[Hex1]],Hex2,_,_,Hive).

% analiza si al quitar la pieza desconecta la colmena
desconecta_colmena(Hex,Hive):-
    cell:axial_neighbors_in_hive(Hex,Hive,L),
    member(H1,L),
    member(H2,L),
    H1 \= H2,
    H1 \= Hex,
    H2 \= Hex,
    delete(Hex,Hive,Hive1),
    not(is_connected(H1,H2,Hive1)),
    !.

is_an_empty_hex(Hex):-
    not(piece(_, _, _, Hex, true,_)),!.

% increment number of moves(Player_id):-
increment_number_of_moves(Player_id):-
    player(Player_id, Name, Number_of_moves, Queen_bee_placed, Game_over),
    Number_of_moves1 is Number_of_moves + 1,
    retract(player(Player_id, Name, Number_of_moves, Queen_bee_placed, Game_over)),
    assert(player(Player_id, Name, Number_of_moves1, Queen_bee_placed, Game_over)).

queen_bee_in_box(Player_id):-
    player(Player_id,_,_,true,_).


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

start_game(Modo, Dificultad):-
    Modo == multiplayer,
    assert(modo(Modo)),
    assert(dificultad(Dificultad)),
    assert(player(p1, 'Player 1', 0, false, false)),
    assert(player(p2, 'Player 2', 0, false, false)),
    assert(current_player(p1)),
    pieces:init_piece(),!.

init_game(Mode, Level):-
    Mode == cpu,
    assert(modo(Modo)),
    assert(dificultad(Dificultad)),
    assert(player(p1, 'Player 1', 0, false, false)),
    assert(player(p2, 'CPU', 0, false, false)),
    assert(current_player(p1)),
    pieces:init_piece().


movimientos_abeja_validos(Player_id,Hex,Valid_move):-
    current_player =:= Player_id,
    hexagonos_ocupados(Hive),
    not(desconecta_colmena(Hex,Hive)),
    cell:axial_neighbors(Hex,Valid_move),
    findall(X,member(X,Valid_move),celda_vacia(X),Valid_move2).

celda_vacia(Hex):-
    not(piece(_,_,_,Hex,true,_)),!.

movimientos_beetle_valido(Player_id,Hex,Valid_move):-
    current_player =:= Player_id,
    hexagonos_ocupados(Hive),
    not(desconecta_colmena(Hex,Hive)),
    cell:axial_neighbors(Hex,Valid_move).

movimiento_hormiga_valido(Player_id,Hex,Valid_move):-
    current_player =:= Player_id,
    hexagonos_ocupados(Hive),
    not(desconecta_colmena(Hex,Hive)),
    bfs_lvl([[Hex,0]],[],22,pred_hormiga),
    findall(U, (node(U, Lvl), Lvl > 0), Valid_move),
    retractall(node(_, _)).

movimiento_arana_valido(Player_id,Hex,Valid_move):-
    current_player =:= Player_id,
    hexagonos_ocupados(Hive),
    not(desconecta_colmena(Hex,Hive)),
    bfs_lvl([[Hex,0]],[],3,pred_hormiga),
    findall(U, (node(U, Lvl), Lvl =:= 3), Valid_move),
    retractall(node(_, _)).

movimiento_saltamontes_valido(Player_id,Hex,Valid_move):-
    current_player =:= Player_id,
    hexagonos_ocupados(Hive),
    not(desconecta_colmena(Hex,Hive)),
    mov_dir_saltamonte(Hex,[0,-1],0,N),
    mov_dir_saltamonte(Hex,[1,0],0,NE),
    mov_dir_saltamonte(Hex,[1,1],0,SE),
    mov_dir_saltamonte(Hex,[0,1],0,S),
    mov_dir_saltamonte(Hex,[-1,-1],0,SO),
    mov_dir_saltamonte(Hex,[-1,0],0,NO),
    union([N,NE,SE,S,SO,NO] ,[], L),
    list:delete_all_occurrences([], L ,L1),
    list:delete_all_occurrences([_,1],L1,L2),
    (L2 =\= [] ->
    Valid_move is L2; fail),!.
    

pred_hormiga(Hex):-
    is_an_empty_hex(Hex),
    is_adj(Hex),!.


is_adj(Hex):-
    axial_neighbors(Hex,Neighbors),
    hexagonos_ocupados(Hive),
    findall(X,member(X,Neighbors),member(X,Hive),Found),
    Found=\=[],
    fail,!.

mov_dir_saltamonte(Hex,Dir,N,Hex_final):-
    (is_an_empty_hex(Hex) -> Hex_final is [Hex,N];  
    N1 is N + 1, 
    cell:axial_move(Hex,Dir,Hex2),    
    mov_dir_saltamonte(Hex2,Dir,N1,Hex_final)).

/*get_piece_mov(Type,Id,PlayerId,Mov):-
    piece(Type,Id,PlayerId,_,false,_),
    list_piece(_,_,PlayerId,_,true,1,Pieces),*/

    

%pregunta si hay una pieza sobre esta
is_blocked(Type,Id,PlayerId,Hex):-
    piece(Type, Id, Pid, Hex, true, Lvl),
    Lvl1 is Lvl+1,
    piece(_, _, _, Hex, true, Lvl1),!.


%el primer player juega en la 0,0
posible_colocacion(PlayerId, Moves, Posibles_colocaciones):-
    atom(PlayerId),
    number(Moves),
    var(Posibles_colocaciones),
    PlayerId == p1,
    Moves == 0,
    Posibles_colocaciones = [[0, 0]],
    !.

%el segundo player juega en las casillas adyacente a la ficha del 1er jugador
posible_colocacion(PlayerId, Moves, Posibles_colocaciones):-
    atom(PlayerId),
    number(Moves),
    var(Posibles_colocaciones),
    PlayerId == p2,
    Moves == 0,
    cell:axial_neighbors([0,0],Posibles_colocaciones),
    !.

posible_colocacion(PlayerId, Moves, Posibles_colocaciones):-
    atom(PlayerId),
    number(Moves),
    var(Posibles_colocaciones),
    list_piece(_,_,PlayerId,_,true,1,Colocaciones1),
    other_player(PlayerId,El_otro),
    todos_vecinos_vacios_colmena(Vacios),
    findall(X,member(X,Vacios),puede_colocarse(El_otro,X),Posibles_colocaciones),!.

    
puede_colocarse(OtroId,Hex):-
    axial_neighbors(Hex,Vecinos),
    findall(X,member(X,Vecinos),piece(_,_,OtroId,X,true,_),Can),
    Can=:=[],!.


% Todos los vecinos vacios
todos_vecinos_vacios_colmena(Void_neighbors):-
    findall(VN, (piece(_, _, _, Hex, true,_), vecinos_vacios_hex(Hex, VN)), Void_neighbors_aux),
    list:flatten_hex(Void_neighbors_aux, Void_neighbors_aux1),
    setof(X, member(X, Void_neighbors_aux1), Void_neighbors),!.

vecinos_vacios_hex(Hex, Void_neighbors):-
    cell:axial_neighbors(Hex, Neighbors),
    findall(H, piece(_, _, _, H, true,_), Hexagons),
    findall(X, (member(X, Neighbors), not(member(X, Hexagons))), Void_neighbors),!.

posibles_movimientos(Type,PlayerId,Id,Hex):-
    queen_bee_in_box(PlayerId),
    switch(
        Type,[
            queen_bee: movimientos_abeja_validos(PlayerId,Hex,Valid_move),
            beetle: movimientos_beetle_valido(PlayerId,Hex,Valid_move),
            grasshopper: mov_dir_saltamonte(PlayerId,Hex,Valid_move),
            soldier_ant: movimiento_hormiga_valido(PlayerId,Hex,Valid_move),
            spider: movimiento_arana_valido(PlayerId,Hex,Valid_move)
        ]).

switch(X, [Val:Goal|Cases]) :-
    (X=Val ->
        call(Goal);
        switch(X, Cases)
    ),!.
