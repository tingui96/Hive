
:- module(list, 
    [element_at/3,
    indexOf/3,
    concat/3,
    isList/1,
    isHex/1,
    element_hex/2,
    flatten_hex/2,
    delete/3,
    last_element/2
    ]).


% element_at(X, L, I).
element_at(X,[X|_],1).
element_at(X,[_|L],K) :- element_at(X,L,K1), K is K1 + 1.

% indexOf(X,L,I)
indexOf(X, [X|_], 1):-!.
indexOf(X, [_|R], I):- indexOf(X, R, I1), !, I is I1+1.

% concat(L1, L2, L3) | L3 = L1 + L2
concat([], X, X):-!.
concat([X|R], Y, [X|Z]):- concat(R, Y, Z).

% isList(L)
isList([]).
isList([_|_]).

% Delete(X, L, R)
delete(X,[X|R],R).
delete(X,[Y|R],[Y|R1]):-delete(X,R,R1).

% delete_all_occurrences(X,L,R)
delete_all_occurrences(_,[],[]).
delete_all_occurrences(X,[X|T],R):-
    delete_all_occurrences(X,T,R),!.
delete_all_occurrences(X,[Y|R],[Y|R1]):-
    delete_all_occurrences(X,R,R1).

% last element
last_element(X,[X|[]]):-!.
last_element(X,[H|T]):-
    last_element(X,T).