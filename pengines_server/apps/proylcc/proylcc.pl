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

put(Content, [RowN, ColN], RowsClues, ColsClues, Grid, NewGrid, RowSat, ColSat):-
	% NewGrid is the result of replacing the row Row in position RowN of Grid by a new row NewRow (not yet instantiated).
	replace(Row, RowN, NewRow, Grid, NewGrid),

	% NewRow is the result of replacing the cell Cell in position ColN of Row by _,
	% if Cell matches Content (Cell is instantiated in the call to replace/5).	
	% Otherwise (;)
	% NewRow is the result of replacing the cell in position ColN of Row by Content (no matter its content: _Cell).			
	(replace(Cell, ColN, _, Row, NewRow), Cell == Content;
	replace(_Cell, ColN, Content, Row, NewRow)),
	
	getColumn(NewGrid, ColN, Column),
	getListElement(RowsClues, RowN, RowClue),
	getListElement(ColsClues, ColN, ColClue),

	(
		validClue(RowClue, NewRow), RowSat = 1;
		RowSat = 0
	),
	(
		validClue(ColClue, Column), ColSat = 1;
		ColSat = 0
	).


getListElement([X | _], 0, X) :- !.
getListElement([_ | Xs], Idx, Element) :-
	K is Idx-1,
	getListElement(Xs, K, Element).

getRow(Grid, RowIdx, Row) :- getListElement(Grid, RowIdx, Row).

getColumn(Grid, ColIdx, Col) :-
	findall(X, (member(Row, Grid), getListElement(Row, ColIdx, X)), Col),
	Col \= [].

% countConsecutiveSymbols(+List, +Symbol, -Count :int)
%
%    Given +List and +Symbol, it counts from the start how many
%    consecutive symbols there are and stores it in -Count.
%
%    For example: ?- countConsecutiveSymbols([1,1,2,3], 1, C, S).
%    C = 2.

countConsecutiveSymbols([], _, 0) :- !.
countConsecutiveSymbols([X | _], Symbol, 0) :- X \== Symbol, !.
countConsecutiveSymbols([X | Xs], Symbol, Count) :- 
	X == Symbol,
	countConsecutiveSymbols(Xs, Symbol, SubCount),
	Count is SubCount+1.

% skipConsecutiveSymbols(+List, +Symbol, -Count :int, -Sublist)
%
%    Given +List and +Symbol, it counts from the start how many
%    consecutive symbols there are and stores it in -Count, while -Sublist
%    has the remaining elements skipping the repeated symbols.
%
%    For example: ?- skipConsecutiveSymbols([1,1,2,3], 1, C, S).
%    C = 2, S=[2,3].

skipConsecutiveSymbols([], _, 0, []) :- !.
skipConsecutiveSymbols([X | Xs], Symbol, 0, [X | Xs]) :- X \== Symbol, !.
skipConsecutiveSymbols([X | Xs], Symbol, Count, Sublist) :- 
	X == Symbol,
	skipConsecutiveSymbols(Xs, Symbol, SubCount, Sublist),
	Count is SubCount+1.

% lineToClue(+Line :list, -Clue :list)
%
%    +Line, represents a Row or a Column.
%    -Clue, is a Clue that represents the line.

lineToClue([], []) :- !.
lineToClue([X | Xs], Clue) :- 
	X \== "#",
	lineToClue(Xs, Clue),
	!.
lineToClue(List, [ Count | Counts ]) :- 
	List = [ X | _ ],
	X == "#",
	skipConsecutiveSymbols(List, "#", Count, SubClue),
	lineToClue(SubClue, Counts).

%   validClue(+Clue, +Line).
%
%   +Clue represents the Clue for the current row/column, list of ints.
%   +Line is a simple List representing a Row or a Column. see getRow/3, getColumn/3.

validClue(Clue, Line) :- lineToClue(Line, Clue).