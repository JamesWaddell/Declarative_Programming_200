% Assignment 3, 159.202, 2018 S2 
% Waddell, James, 16379344, 
% A two player version of connect 4.

% Play Connect-Four predicate.
play_connect4 :- startingBoard(Board), connect4(Board, 'x').

% Definition of an empty board (7 empty colums).
startingBoard([[_,_,_,_,_,_],[_,_,_,_,_,_],[_,_,_,_,_,_],[_,_,_,_,_,_],[_,_,_,_,_,_],[_,_,_,_,_,_],[_,_,_,_,_,_]]).

% Main predicate.
connect4(Board, _) :- 
		win(Board, Player, Result),
		Result = 'w',
		write('Player '),
		write(Player),
		write(' Wins!').		
connect4(Board, _) :- 
		win(Board, _, Result),
		Result = 'd',
		write('Draw!').
connect4(Board, Player1) :-
		get_move(Col, Row, Board, Player1),		% Read a column Col from user and calculate Row (not complete)
		set(Col, Row, Board, Player1),			% Set the Board at Col, Row to Player1
		print(Board),							% Print out the Board
		change_player(Player1, Player2),		% Swap to other player								
		connect4(Board, Player2).				% Play the next turn.
		
% Changes the player.		
change_player('x','o').
change_player('o','x').
	
% Get a Column and Row for a player Move.
get_move(Col, Row, Board, Player1) :-
		repeat,	
		nl,			
		write('Player '),
		write(Player1),
		write(' enter move:'),
		nl,
		read(Col),								% Read Column number
		validate(Col),							% Validate input 
		find_first_empty_row(1, Col, Board, X),	% Find first empty Row
		Row = X.										
			
% Finds the first empty row for the column read in.
find_first_empty_row(Row, Col,Board, Result):- 
		Row = 1,
		empty(Col, Row, Board),
		Result = Row.	
find_first_empty_row(Row,Col,Board, Result):- 
		Row >1,
		Row <7,
		empty(Col, Row, Board),
		Result = Row.	
find_first_empty_row(Row,Col,Board, Result):- 
		Row < 6,
		not(empty(Col,Row,Board)),
		X is Row + 1,
		find_first_empty_row(X, Col, Board, Result).	
find_first_empty_row(Row, Col,Board, _):- 
		Row = 6,
		not(empty(Col,Row,Board)),
		% add player prompt here
		write('The current column is full. Please try again.'),
		nl,
		fail.
		
% Validates the input.
validate(X):-
		integer(X),
		X>0,
		X<8.
validate(X):-
		integer(X), 
		(X>7;X<1),
		write(X), write(' is not a valid column (must be a number in the range [1-7]).'),
		nl,
		fail.
validate(X):-
		not(integer(X)), write(X),
		write(' is not a valid column (must be a number in the range [1-7]).'),
		nl,
		fail.
	
% Test whether Board is empty at column Col and row Row.
empty(Col, Row, Board) :-
		nth_item(Col, Board, Column),	% Find the column in the board
		nth_item(Row, Column, Item),	% Find the item in the column
		var(Item).						% Check the item is still a variable

% Set the Board at column Col and row Row to Item.
set(Col, Row, Board, Item) :-
	nth_item(Col, Board, Column),	% Find the column in the board
	nth_item(Row, Column, Item).	% Find the item in the board and match with Item

% Get the Item in the Board at column Col and row Row.
get(Col, Row, Board, Item) :-
	nth_item(Col, Board, Column),	% Find the column in the board 
	nth_item(Row, Column, Item),	% Find the item in the column
	nonvar(Item).					% Check it isn't a variable

% Win Conditions. 
win(Board, _, Result):-
		full(1, 6, Board),	% Draw
		Result = 'd'.
win(Board, Player, Result) :-
		v_win(1, 1, Board, Player),	% Player wins vertically
		Result = 'w'.				
win(Board, Player, Result):-
		h_win(1, 1, Board, Player),
		Result = 'w'.				% Player wins horizontally  
win(Board, Player, Result):- 
		d_win_l(7, 1, Board, Player),
		Result = 'w'.					% Player wins diagonally to the left 
win(Board, Player, Result):- 
		d_win_r(1, 1, Board, Player),	% Player wins diagonally to the right 
		Result = 'w'.

% Check if a player has won vertically.			
v_win(Col, Row, Board, Player):-
			Row<4,
			check_next_three_rows(Col, Row, Board, Player).
v_win(Col, Row, Board, Player):-
			Row<4,
			X is Row +1,
			v_win(Col, X, Board, Player).
v_win(Col, Row, Board, Player):-
			Row>=4,
			Col<7,
			X is Col+1,
			v_win(X, 1, Board, Player).
			
% Check next three rows are the same as the current row.					
check_next_three_rows(Col, Row, Board, Player):-
		get(Col, Row, Board, I1),
		Rp1 is Row+1,
		get(Col, Rp1, Board, I2),
		Rp2 is Rp1+1,
		get(Col, Rp2, Board, I3),
		Rp3 is Rp2+1,
		get(Col, Rp3, Board, I4),
		Player = I1,
		Player = I2,
		Player = I3,
		Player = I4.

% Check if a player has won horizontally.	
h_win(Col, Row, Board, Player):-
		Col<5,
		check_next_three_cols(Col, Row, Board, Player).
h_win(Col, Row, Board, Player):-
		Col<5,
		X is Col+1,
		h_win(X, Row, Board, Player).
		
h_win(Col, Row, Board, Player):-
		Col>=5,
		Row<6,
		X is Row+1,
		h_win(1, X, Board, Player).	

% Check next three columns are the same as the current column.		
check_next_three_cols(Col, Row, Board, Player):-
		get(Col, Row, Board, I1),
		Cp1 is Col+1,
		get(Cp1, Row, Board, I2),
		Cp2 is Cp1+1,
		get(Cp2, Row, Board, I3),
		Cp3 is Cp2+1,
		get(Cp3, Row, Board, I4),
		Player = I1,
		Player = I2,
		Player = I3,
		Player = I4.
		
% Check if a player has won diagonally to the left.
d_win_l(Col, Row, Board, Player):-
		Col>3,
		Row<4,
		check_next_three_diag_l(Col, Row, Board, Player).
d_win_l(Col, Row, Board, Player):-
		Col>3,
		X is Col-1,
		d_win_l(X, Row, Board, Player).
d_win_l(Col, Row, Board, Player):-
		Col=<3,
		Row<4,
		X is Row+1,
		d_win_l(7, X, Board, Player).	
		
% Check next three in a diagonal to the left are the same as the current positon.		
check_next_three_diag_l(Col, Row, Board, Player):-
		get(Col, Row, Board, I1),	
		Cp1 is Col-1,
		Rp1 is Row +1,
		get(Cp1, Rp1, Board, I2),
		Cp2 is Cp1-1,
		Rp2 is Rp1 +1,
		get(Cp2, Rp2, Board, I3),
		Cp3 is Cp2-1,
		Rp3 is Rp2 +1,
		get(Cp3, Rp3, Board, I4),
		Player = I1,
		Player = I2,
		Player = I3,
		Player = I4.
		
% Check if a player has won diagonally to the right.		
d_win_r(Col, Row, Board, Player):-
		Col<5,
		Row<4,
		check_next_three_diag_r(Col, Row, Board, Player).
d_win_r(Col, Row, Board, Player):-
		Col<5,
		X is Col+1,
		d_win_r(X, Row, Board, Player).
d_win_r(Col, Row, Board, Player):-
		Col>=5,
		Row<4,
		X is Row+1,
		d_win_r(1, X, Board, Player).
	
% Checks if the next three in a diagonal to the right are the same as the current positon.		
check_next_three_diag_r(Col, Row, Board, Player):-
		get(Col, Row, Board, I1),	
		Cp1 is Col+1,
		Rp1 is Row +1,
		get(Cp1, Rp1, Board, I2),
		Cp2 is Cp1+1,
		Rp2 is Rp1 +1,
		get(Cp2, Rp2, Board, I3),
		Cp3 is Cp2+1,
		Rp3 is Rp2 +1,
		get(Cp3, Rp3, Board, I4),
		Player = I1,
		Player = I2,
		Player = I3,
		Player = I4.


% Checks to see if board is full(a draw).		
full(Col, Row, Board):-
		Col<7,
		not(empty(Col, Row, Board)),
		X is Col +1,
		full(X, Row , Board).		
full(Col, Row, Board):-
		Col = 7,
		not(empty(Col, Row, Board)).

% Get the nth item in a list (index starts from 1).
nth_item(1, [H|_], H).
nth_item(N, [_|T], E) :-
		N > 1,
		N1 is N-1,
		nth_item(N1, T, E).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Print predicates (simplified) %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Print the Connect-Four Board.
print(Board) :- printRows(Board, 6), write('---------------'), nl.

% Print Rows.
printRows(_, 0).
printRows(Board, N) :-
		N > 0,
		printRow(Board, N),
		N1 is N - 1,
		printRows(Board, N1).

% Print a Row.
printRow(Board, Row) :- 
		printCols(Board, Row, 7),
		write('|'),		
		nl.

% Print Columns.
printCols(_, _, 0).
printCols(Board, Row, Col) :-
		Col > 0,
		Col1 is Col - 1,
		printCols(Board, Row, Col1),
		printItem(Board, Row, Col).

% Print an Item.
printItem(Board, Row, Col) :-
		empty(Col, Row, Board),
		write('| ').
printItem(Board, Row, Col) :-
		get(Col, Row, Board, Item),
		write('|'),
		write(Item).
