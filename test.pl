% Este es mi primer programa en Prolog
%
% Se trata de un árbol genealógico muy simple
%%
% Primero defino los parentescos básicos
% de la familia.
% padre(A,B) significa que B es el padre de A...
padre(juan,alberto).
padre(luis,alberto).
padre(alberto,leoncio).
padre(alberto,julio).
padre(geronimo,leoncio).
padre(luisa,geronimo).
% Ahora defino las condiciones para que
% dos individuos sean hermanos
% hermano(A,B) significa que A es hermano de B...
hermano(A,B) :-
padre(A,P),
padre(B,P),
A \== B.
% Ahora defino el parentesco abuelo-nieto.
% nieto(A,B) significa que A es nieto de B...
nieto(A,B) :-
padre(A,P),
padre(P,B).


state_record(State, Parent, [State, Parent]).

go(Start, Goal) :- 
    empty_queue(Empty_open),
    state_record(Start, nil, State),
    add_to_queue(State, Empty_open, Open),
    empty_set(Closed),
    path(Open, Closed, Goal).

path(Open,_,_) :- empty_queue(Open),
                  write('graph searched, no solution found').

path(Open, Closed, Goal) :- 
    remove_from_queue(Next_record, Open, _),
    state_record(State, _, Next_record),
    State = Goal,
    write('Solution path is: '), nl,
    printsolution(Next_record, Closed).

path(Open, Closed, Goal) :- 
    remove_from_queue(Next_record, Open, Rest_of_open),
    (bagof(Child, moves(Next_record, Open, Closed, Child), Children);Children = []),
    add_list_to_queue(Children, Rest_of_open, New_open), 
    add_to_set(Next_record, Closed, New_closed),
    path(New_open, New_closed, Goal),!.

moves(State_record, Open, Closed, Child_record) :-
    state_record(State, _, State_record),
    mov(State, Next),
    % not (unsafe(Next)),
    state_record(Next, _, Test),
    not(member_queue(Test, Open)),
    not(member_set(Test, Closed)),
    state_record(Next, State, Child_record).

printsolution(State_record, _):- 
    state_record(State,nil, State_record),
    write(State), nl.



