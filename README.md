# TicTacProlog
## Cómo correr la aplicación
Utilizando SWI-Prolog, abre el archivo usando consult.
Corre el siguiente comando
```
start.
```
## Test Cases
### Casos en los que gana la computadora
### Casos en los que gana quien juega
### Casos en los que hay empate


## Documentación
### UI
Para el UI, se utiliza la librería xpce, tabular para hacer una tabla más eficientemente y autowin para modificar el tamaño de la ventana automáticamente.
```
:- use_module(library(pce)).
:- use_module(library(tabular)).
:- use_module(library(autowin)).
```
Se crea el display de la tabla en el predicado `make_table` y se conecta con el resto de los predicados de lógica.
Cada `send` manda datos al UI, como la creación de los botones y de sus respectivos labels.
```
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

    send(Button1, message, message(@prolog, button_clicked, 1, Button1)),
    send(Button2, message, message(@prolog, button_clicked, 2, Button2)),
    send(Button3, message, message(@prolog, button_clicked, 3, Button3)),
    send(Button4, message, message(@prolog, button_clicked, 4, Button4)),
    send(Button5, message, message(@prolog, button_clicked, 5, Button5)),
    send(Button6, message, message(@prolog, button_clicked, 6, Button6)),
    send(Button7, message, message(@prolog, button_clicked, 7, Button7)),
    send(Button8, message, message(@prolog, button_clicked, 8, Button8)),
    send(Button9, message, message(@prolog, button_clicked, 9, Button9)),

    retractall(button_mapping(_)),
    asserta(button_mapping([Button1, Button2, Button3, Button4, Button5, Button6, Button7, Button8, Button9])),

    send(P, open).
```
## Explicación del código
#### Importar las librerías necesarias
```
:- use_module(library(pce)).
:- use_module(library(tabular)).
:- use_module(library(autowin)).
```
El primer paso es importar las librerías que se utilizan para el UI.

#### Definición del cuadrado mágico
```
magic_square([2, 7, 6, 
              9, 5, 1, 
              4, 3, 8]).
```
El segundo paso es implementar un cuadrado mágico. 
La suma de todas sus columnas, filas y diagonales da 15. 
Se utilizará para las condición de éxito.

#### Estado inicial
```
initial_board([_,_,_,
              _,_,_,
              _,_,_]).
```
El estado inicial es una lista completamente vacía que representa al tablero.

#### Revisar si el tablero está lleno
```
board_full(Board) :-
    \+ member(_, Board).
```
Esta sentencia revisa que no haya variables libres en el tablero.
Si hay aunque sea una variable libre, significa que el tablero no está lleno, por lo que regresa falso.

#### Sumar la lista 
```
sum_list([], 0).
sum_list([H|T], Sum) :- sum_list(T, Rest), Sum is H + Rest.
```
Esta es una función recursiva. Infiere que la suma total de elementos en la lista será la cabeza más el resto.
Cuando no quedan elementos en la lista, la recursión termina. Entonces se combinan los resultados y se encuentra el total.

#### Combinación ganadora
```
winning_combination(Positions) :-
    magic_square(Square),
    findall(Value, (member(Pos, Positions), nth1(Pos, Square, Value)), Values),
    sum_list(Values, 15).
```
Esta función recibe una lista de posiciones. Después manda a llamar el cuadro mágico.
Suma las posiciones utilizando el cuadro mágico. 
En caso de que sea una combinación ganadora, la suma será de 15.

#### Revisar si jugador actual ganó
```
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
```
Esta función recibe la marca (o, x) que está usando el jugador actual y el tablero.
Al principio se definen las posibles combinaciones con las que se puede ganar.
Se comparan las marcas el tablero con cada una de las posiciones ganadoras. 
Si las 3 marcas en las 3 posiciones ganadoras son idénticas a la del jugador actual, significa que ganó.
Regresa verdadero si el jugador actual ganó.

#### Hacer movimiento (Jugador)
```
make_move(Board, Position, Player, NewBoard) :-
    nth1(Position, Board, _, Rest),
    nth1(Position, NewBoard, Player, Rest).
```
Se definió esta función para permitir que el jugador realice un movimiento.
Genera un tablero nuevo tablero en el que se agrega la marca actual (x,o).

### Elegir movimiento (Computadora)
```
choose_move(Board, Position) :-
    findall(Pos-MagicValue, (between(1, 9, Pos), valid_move(Board, Pos), magic_square_value(Pos, MagicValue)), ValidMoves),
    keysort(ValidMoves, SortedMoves),
    member(Position-_, SortedMoves).
```
La función genera valores válidos de posiciones que podría elegir la computadora.
Revisa que estén entre 1 y 9 y que la posición no esté ocupada.
Junta esas posiciones con el valor que tendrían en el cuadro mágico. 
En base a los valores del cuadro mágico, elige el mejor movimiento.

#### Valor del cuadrado mágico
```
magic_square_value(Position, Value) :-
    magic_square(Square),
    nth1(Position, Square, Value).
```
Esta función regresa el valor que tiene una determinada posición en el cuadro mágico.

#### Movimiento válido
```
valid_move(Board, Position) :- 
    nth1(Position, Board, Value),
    var(Value).
```
Este predicado revisa que la posición no esté ocupada.

#### Cambio de jugador
```
switch_player(x, o).
switch_player(o, x).
```
Una función que cambia el jugador.

#### Tablero, jugador y mapa dinámicos
```
:- dynamic current_board/1.
:- dynamic current_player/1.
:- dynamic button_mapping/1.
```
Se declaran como predicados dinámicos para que su valor pueda cambiar durante el juego.

#### Inicializar el juego
```
initialize_game :-
    initial_board(Board),
    retractall(current_board(_)),
    asserta(current_board(Board)),
    retractall(current_player(_)),
    asserta(current_player(x)),
    retractall(button_mapping(_)),
    asserta(button_mapping([])).
```
El juego se inicializa con el tablero inicial, se elige la marca del jugador (en este caso x) y 
se borra la asignación de botones.

#### Comenzar el juego
```
start :-
    initialize_game,
    make_table.
```
Aquí se inicializa el juego y se manda a llamar la interfaz gráfica mencionada al inicio del documento.

#### Al presionar un botón
```
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
```
Esta función detecta cuando un botón fue presionado por el jugador.
Agrega el movimiento del jugador al tablero y revisa si alguien ganó o hubo empate.
Después hace el cambio de jugador.

#### Movimiento de la computadora
```
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
```
Manda a llamar la función anteriormente descrita, que busca el mejor movimiento siguiente utilizando el cuadro mágico.
En base a esto hace un movimiento. Éste se agrega al tablero y de igual manera se revisa si alguien ganó o si hubo empate.
Una vez hecho el movimiento, se hace el cambio de jugador.

#### Terminar juego
```
disable_all_buttons :-
    button_mapping(Buttons),
    maplist({@off}/[Button]>>send(Button, active, @off), Buttons).
```
En cuanto se termina el juego se deshabilitan todos los botones.

#### Comando inicial para comenzar el juego
```
:- start.
```
Este comando se utiliza para comenzar el juego completo.
