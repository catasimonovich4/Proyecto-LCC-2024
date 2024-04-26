:- module(proylcc, [put/8]).
:- use_module(library(lists)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% replace(?X, +XIndex, +Y, +Xs, -XsY)
%
% XsY is the result of replacing the occurrence of X in position XIndex of Xs by Y.

replace(X, 0, Y, [X|Xs], [Y|Xs]).

replace(X, XIndex, Y, [Xi|Xs], [Xi|XsY]):-
    XIndex > 0,
    XIndexS is XIndex - 1,
    replace(X, XIndexS, Y, Xs, XsY).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% put(+Content, +Pos, +RowsClues, +ColsClues, +Grid, -NewGrid, -RowSat, -ColSat).
%

put(Content, [RowN, ColN], _RowsClues, _ColsClues, Grid, NewGrid, 0, 0):-
	% NewGrid is the result of replacing the row Row in position RowN of Grid by a new row NewRow (not yet instantiated).
	replace(Row, RowN, NewRow, Grid, NewGrid),

	% NewRow is the result of replacing the cell Cell in position ColN of Row by _,
	% if Cell matches Content (Cell is instantiated in the call to replace/5).	
	% Otherwise (;)
	% NewRow is the result of replacing the cell in position ColN of Row by Content (no matter its content: _Cell).			
	(replace(Cell, ColN, _, Row, NewRow), Cell == Content;
	replace(_Cell, ColN, Content, Row, NewRow)).


getListElement(0, [X | _], X) :- !.

getListElement(Idx, [_ | Xs], Element) :-
	K is Idx-1,
	getListElement(K, Xs, Element).

getRow(RowIdx, Grid, Row) :- getListElement(RowIdx, Grid, Row).

getColumn(ColIdx, Grid, Col) :-
	findall(X, (member(Row, Grid), getListElement(ColIdx, Row, X)), Col),
	Col \= [].


%   validClue(+Clue, +List).
%
%   +Clue represents the Clues for the current row/column.
%   +List is a simple List representing a Row or a Column. see GetRow/3, GetColumn/3.
validClue(0, ["X" | Xs]).
validClue(Clue, List).
