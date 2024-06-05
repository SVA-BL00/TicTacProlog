:- use_module(library(pce)).
:- use_module(library(tabular)).
:- use_module(library(autowin)).

% Define the positions in the magic square
magic_square([2, 7, 6, 
              9, 5, 1, 
              4, 3, 8]).

% Define the initial empty board
initial_board([_, _, _, 
               _, _, _, 
               _, _, _]).

% Check if the board is full
board_full(Board) :-
    \+ member(_, Board).

% Sum the values in a list
sum_list([], 0).
sum_list([H|T], Sum) :- sum_list(T, Rest), Sum is H + Rest.

% Check if a list of positions sum to 15
winning_combination(Positions) :-
    magic_square(Square),
    findall(Value, (member(Pos, Positions), nth1(Pos, Square, Value)), Values),
    sum_list(Values, 15).

% Check all possible winning combinations
winner(Board, Player) :-
    WinningPositions = [
        [1, 2, 3], [4, 5, 6], [7, 8, 9],  % Rows
        [1, 4, 7], [2, 5, 8], [3, 6, 9],  % Columns
        [1, 5, 9], [3, 5, 7]              % Diagonals
    ],
    member(Pos, WinningPositions),
    nth1(1, Pos, P1), nth1(2, Pos, P2), nth1(3, Pos, P3),
    nth1(P1, Board, Mark1), nth1(P2, Board, Mark2), nth1(P3, Board, Mark3),
    nonvar(Mark1), nonvar(Mark2), nonvar(Mark3), % Ensure all marks are bound
    Mark1 = Mark2, Mark2 = Mark3, Mark3 = Player.

% Helper predicate to check if all elements in a list are the same and not unbound
all_same([H|T], H) :-
    (nonvar(H) -> all_same_nonvar(T, H) ; all_same_var(T)).
all_same([], _).

% Case when the head is a non-variable
all_same_nonvar([H|T], H) :- all_same_nonvar(T, H).
all_same_nonvar([], _).

% Case when the head is a variable
all_same_var([H|T]) :- var(H), all_same_var(T).
all_same_var([]).

% Make a move on the board
make_move(Board, Position, Player, NewBoard) :-
    nth1(Position, Board, _, Rest),
    nth1(Position, NewBoard, Player, Rest).

% Choose a move based on the magic square logic
choose_move(Board, Position) :-
    findall(Pos-MagicValue, (between(1, 9, Pos), valid_move(Board, Pos), magic_square_value(Pos, MagicValue)), ValidMoves),
    keysort(ValidMoves, SortedMoves),
    member(Position-_, SortedMoves).

% Calculate the value of the magic square at a given position
magic_square_value(Position, Value) :-
    magic_square(Square),
    nth1(Position, Square, Value).

% Check if a move is valid
valid_move(Board, Position) :- 
    nth1(Position, Board, Value),
    var(Value).

% Switch players
switch_player(x, o).
switch_player(o, x).

% GUI Integration

:- dynamic current_board/1.
:- dynamic current_player/1.
:- dynamic button_mapping/1.

% Define the initial state
initialize_game :-
    initial_board(Board),
    retractall(current_board(_)),
    asserta(current_board(Board)),
    retractall(current_player(_)),
    asserta(current_player(x)),
    retractall(button_mapping(_)),
    asserta(button_mapping([])).

% Start the game
start :-
    initialize_game,
    make_table.

make_table :-
    new(P, auto_sized_picture('Tic tac toe')),
    send(P, display, new(T, tabular)),
    send(T, border, 1),
    send(T, cell_spacing, -1),
    send(T, rules, all),
    send_list(T, [
        append(new(Button1, button(''))),
        append(new(Button2, button(''))),
        append(new(Button3, button(''))),
        next_row,
        append(new(Button4, button(''))),
        append(new(Button5, button(''))),
        append(new(Button6, button(''))),
        next_row,
        append(new(Button7, button(''))),
        append(new(Button8, button(''))),
        append(new(Button9, button('')))
    ]),

    % Define the action for each button
    send(Button1, message, message(@prolog, button_clicked, 1, Button1)),
    send(Button2, message, message(@prolog, button_clicked, 2, Button2)),
    send(Button3, message, message(@prolog, button_clicked, 3, Button3)),
    send(Button4, message, message(@prolog, button_clicked, 4, Button4)),
    send(Button5, message, message(@prolog, button_clicked, 5, Button5)),
    send(Button6, message, message(@prolog, button_clicked, 6, Button6)),
    send(Button7, message, message(@prolog, button_clicked, 7, Button7)),
    send(Button8, message, message(@prolog, button_clicked, 8, Button8)),
    send(Button9, message, message(@prolog, button_clicked, 9, Button9)),

    % Store button mapping for later updates
    retractall(button_mapping(_)),
    asserta(button_mapping([Button1, Button2, Button3, Button4, Button5, Button6, Button7, Button8, Button9])),

    send(P, open).

% Define what happens when a button is clicked
button_clicked(Position, Button) :-
    current_board(Board),
    current_player(Player),
    (valid_move(Board, Position) ->
        make_move(Board, Position, Player, NewBoard),
        send(Button, label, Player),
        send(Button, active, @off),
        retract(current_board(_)),
        asserta(current_board(NewBoard)),
        (winner(NewBoard, Player) ->
            format("Player ~w wins!\n", [Player]),
            disable_all_buttons
            ;
            (board_full(NewBoard) ->
                format("It's a draw!\n"),
                disable_all_buttons
                ;
                switch_player(Player, NextPlayer),
                retract(current_player(_)),
                asserta(current_player(NextPlayer)),
                (NextPlayer = o -> 
                    choose_move(NewBoard, CompPosition),
                    button_mapping(Buttons),
                    nth1(CompPosition, Buttons, CompButton),
                    computer_move(CompPosition, CompButton)
                    ;
                    true
                )
            )
        )
        ;
        format("Invalid move. Try again.\n")
    ).

% Define what happens when the computer makes a move
computer_move(Position, Button) :-
    current_board(Board),
    current_player(Player),
    Player = o,
    (valid_move(Board, Position) ->
        make_move(Board, Position, Player, NewBoard),
        send(Button, label, Player),
        send(Button, active, @off),
        retract(current_board(_)),
        asserta(current_board(NewBoard)),
        (winner(NewBoard, Player) ->
            format("Player ~w wins!\n", [Player]),
            disable_all_buttons
            ;
            (board_full(NewBoard) ->
                format("It's a draw!\n"),
                disable_all_buttons
                ;
                switch_player(Player, NextPlayer),
                retract(current_player(_)),
                asserta(current_player(NextPlayer))
            )
        )
    ).

% Disable all buttons (end game)
disable_all_buttons :-
    button_mapping(Buttons),
    maplist({@off}/[Button]>>send(Button, active, @off), Buttons).

% Initialize and start the game
:- start.

