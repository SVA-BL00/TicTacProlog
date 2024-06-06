# TicTacProlog
## Cómo correr la aplicación
Utilizando SWI-Prolog, abre el archivo usando consult.
Corre el siguiente comando
```
start.
```
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
### Representación del juego
####Estado inicial
        initial_board([_, _, _,
                   _, _, _,
                   _, _, _]).
        El estado inicial es una lista completamente vacía que representa el tablero.



## Test Cases
