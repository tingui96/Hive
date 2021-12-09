:- module(cell,
    [
        direction/3,
        axial_neighbors/2,
        are_neighbors/2,
        anti_neighborhood/3,
        get_QR/3,
        is_a_hinged_hex/2
    ]).

%IMPORTAR MODULO LIST
:-consult(list), import(list).

%direcciones en donde se puedan encontrar celdas vecinas
%distribucion flat top
%axial
%************************
%***********____*********
%**********/    \********
%**********\____/********
%************************
direction(0, norte, [0, -1]).
direction(1, noreste, [1, 0]).
direction(2, sureste, [1, 1]).
direction(3, sur, [0, 1]).
direction(4, suroeste, [-1, -1]).
direction(5, noroeste, [-1, 0]).


%Conversion de los ejes de coordenadas
% cube_to_axial(Cube, Hex)
cube_to_axial([Q, R, _], Hex):-
    number(Q),
    number(R),
    var(Hex),
    Hex = [Q, R].

% axial_to_cube(Hex, Cube)
axial_to_cube([Q, R], Cube):-
    number(Q),
    number(R),
    var(Cube),
    S is -Q-R,
    Cube = [Q, R, S].

%Convertir a offset coord (flat top)
% axial_to_oddr(Hex, Offset)
axial_to_oddq([Q, R], Offset):-
    number(Q),
    number(R),
    var(Offset),
    Col = Q,
    Row is R + (Q - (Q mod 2)) / 2,
    Offset = [Col, Row].

% oddq_to_axial(Offset, Hex)
oddq_to_axial([Col, Row], Hex):-
    number(Col),
    number(Row),
    var(Hex),
    Q = Col,
    R is Row - (Col - (Col mod 2)) /2,
    Hex = [Q, R].


%Calcular los vecinos de una celda
%Calcula los vectores de direcciones donde puede esta otra celda
% axial_direction_vectors(Adv)
axial_direction_vectors(Adv):-
    findall(Hex, direction(_, _, Hex), Adv).

%
% axial_direction(Dir, Axial_direction_vector)
axial_direction(Dir, Vec):-
    axial_direction_vectors(Adv),
    list:element_at(Vec, Adv, Dir).

% axial_move(Hex1, Vec, Hex2)
axial_move([Q1, R1], [Q2, R2], Hex):-
    Q3 is Q1 + Q2,
    R3 is R1 + R2,
    Hex = [Q3, R3].

% axial_neighbor(Hex, Dir, Neighbor)
axial_neighbor(Hex, Dir, Neighbor):-
    axial_direction(Dir, Vec),
    axial_move(Hex, Vec, Neighbor).

%Calcula los vecinos de una celda
% axial_neighbors(Hex, Neighbors)
axial_neighbors(Hex, Neighbors):-
    findall(Neighbor, axial_neighbor(Hex, _, Neighbor), Neighbors).

% true if Hex1 and Hex2 are neighbors.
are_neighbors(Hex1, Hex2):-
    axial_neighbors(Hex1, N),
    member(Hex2, N),!.


