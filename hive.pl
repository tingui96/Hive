:- module(hive, 
    [
        piece/5,
        list_piece/6,
        init_piece/0,
        set_piece/5,
    ]).

:-consult(cell), import(cell).

:-dynamic piece/6.
% piece(Type, Id, PlayerId, Hex=[Q,R], Placed)

% list of pieces with filter
list_piece(Type, Id, Player_id, Hex, Placed, Insects):-
    findall([Type, Id, Player_id, Hex, Placed], piece(Type, Id, Player_id, Hex, Placed), Pieces).

% initialize insects with default values
init_piece():-
    assert(piece(queen_bee, 1, p1, none, false)),
    assert(piece(queen_bee, 1, p2, none, false)),

    assert(piece(beetle, 1, p1, none, false)),
    assert(piece(beetle, 2, p1, none, false)),
    assert(piece(beetle, 1, p2, none, false)),
    assert(piece(beetle, 2, p2, none, false)),

    assert(piece(grasshopper, 1, p1, none, false)),
    assert(piece(grasshopper, 2, p1, none, false)),
    assert(piece(grasshopper, 3, p1, none, false)),
    assert(piece(grasshopper, 1, p2, none, false)),
    assert(piece(grasshopper, 2, p2, none, false)),
    assert(piece(grasshopper, 3, p2, none, false)),

    assert(piece(spider, 1, p1, none, false)),
    assert(piece(spider, 2, p1, none, false)),
    assert(piece(spider, 1, p2, none, false)),
    assert(piece(spider, 2, p2, none, false)),

    assert(piece(soldier_ant, 1, p1, none, false)),
    assert(piece(soldier_ant, 2, p1, none, false)),
    assert(piece(soldier_ant, 3, p1, none, false)),
    assert(piece(soldier_ant, 1, p2, none, false)),
    assert(piece(soldier_ant, 2, p2, none, false)),
    assert(piece(soldier_ant, 3, p2, none, false)),

    assert(piece(ladybug, 1, p1, none, false)),
    assert(piece(ladybug, 1, p2, none, false)),

    assert(piece(mosquito, 1, p1, none, false)),
    assert(piece(mosquito, 1, p2, none, false)),

    assert(piece(pillbug, 1, p1, none, false)),
    assert(piece(pillbug, 1, p2, none, false)).

% place insect in Hex
% set_piece(Player_id, Type, Hex, Insect)
set_piece(Player_id, Type, Hex, Insect):-
    var(Insect),
    atom(Player_id),
    atom(Type),
    compound(Hex),
    var(Insect),
    piece(Type, Id, Player_id, none, false),
    !,
    retract(piece(Type, Id, Player_id, none, false)),
    assert(piece(Type, Id, Player_id, Hex, true)),
    piece(Type, Id, Player_id, Hex, true)=..Insect.

%Algoritmo DFS
dfs_visit([[Goal|Path]|_],Goal,[Goal|Path],0,_):-!.
dfs_visit([Path|Queue],Goal,FinalPath,N,Hive) :-
    extend(Path,NewPaths, Hive),
    append(NewPaths,Queue,NewQueue),
    dfs_visit(NewQueue,Goal,FinalPath,M,Hive),
    N is M+1.

% tells if there is a path in the graph that joins the two hexagons (if they are connected)
is_connected(Hex1,Hex2,Hive):-
    dfs_visit([[Hex1]],Hex2,_,_,Hive).

% returns list with neighbors placed in the hive
axial_neighbors_in_hive(Hex,Hive,L):-
    cell:axial_neighbors(Hex, Neighbors),
    findall(X,(member(X,Neighbors),member(X,Hive)), L).

% a hexagon is hinged if it is a hinge node in the graph formed by all hexagons...
% I remove the candidate hexagon to point of articulation 
% and analyze if all its neighbors are still connected
is_a_hinged_hex(Hex,Hive):-
    axial_neighbors_in_hive(Hex,Hive,L),
    member(H1,L),
    member(H2,L),
    H1 \= H2,
    H1 \= Hex,
    H2 \= Hex,
    utils:delete(Hex,Hive,Hive1),
    not(is_connected(H1,H2,Hive1)),
    !.
