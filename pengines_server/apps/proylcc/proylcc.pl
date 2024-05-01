:- module(proylcc, [put/8, getClueStates/5]).
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
	lineToClue(NewRow, RowClue), RowSat = 1;
	RowSat = 0
	),
	(
		lineToClue(Column, ColClue), ColSat = 1;
		ColSat = 0
	).
	

/******************************************************************************** 
 * getListElement(+List, +Idx, -Element).
 * 
 * Succeeds if the {Idx} element of {List} is equal to {Element}.
*/
getListElement([X | _], 0, X) :- !.
getListElement([_ | Xs], Idx, Element) :-
	K is Idx-1,
	getListElement(Xs, K, Element).

getRow(Grid, RowIdx, Row) :- getListElement(Grid, RowIdx, Row).

getColumn(Grid, ColIdx, Col) :-
	findall(X, (member(Row, Grid), getListElement(Row, ColIdx, X)), Col),
	Col \= [].

getAmountRows(Grid, AmoutRows) :- length(Grid, AmoutRows).

getAmountColumns([X | _], AmountColumns) :- length(X, AmountColumns).

/******************************************************************************** 
 * countConsecutiveSymbols(+List, +Symbol, -Count :int).
 * 
 * Succeeds if the first {Count} elements of {List} are equal to {Symbol}.
*/
countConsecutiveSymbols([], _, 0) :- !.
countConsecutiveSymbols([X | _], Symbol, 0) :- X \== Symbol, !.
countConsecutiveSymbols([X | Xs], Symbol, Count) :- 
	X == Symbol,
	countConsecutiveSymbols(Xs, Symbol, SubCount),
	Count is SubCount+1.

/******************************************************************************** 
 * skipConsecutiveSymbols(+List, +Symbol, -Count :int, -Sublist).
 * 
 * Succeeds if the first {Count} elements of {List} are equal to {Symbol},
 * and the sublist containing the remaining {length(List)-Count} elements is equal to {Sublist}.
*/
skipConsecutiveSymbols([], _, 0, []) :- !.
skipConsecutiveSymbols([X | Xs], Symbol, 0, [X | Xs]) :- X \== Symbol, !.
skipConsecutiveSymbols([X | Xs], Symbol, Count, Sublist) :- 
	X == Symbol,
	skipConsecutiveSymbols(Xs, Symbol, SubCount, Sublist),
	Count is SubCount+1.


/******************************************************************************** 
 * lineToClue(+Line :list, -Clue :list).
 * 
 * Succeeds if the {Line} representation is equal to the {Clue} representation.
 * 
 * @param Line is a list which contains "#" or anything else(_). @see getRow/3, getColumn/3.
 * @param Clue is a list of ints representing a Line,
 * 			   such that every int is the amount of consecutive "#"s
 * 			   followed or preceded by any amount of other elements(_).
*/
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

/******************************************************************************** 
 * getClueStates(+Grid, +RowsClues, +ColumnsClues, -RowsCluesStates, -ColumnsCluesStates)
 * 
 * Succeeds if:
 *     {RowsCluesStates}    is the list of booleans which represents the satisfied rows    in {Grid}  and
 *     {ColumnsCluesStates} is the list of booleans which represents the satisfied columns in {Grid}.
 * the N row    in {grid} is satisfied if it is equivalent in its Clue representation to the N Clue of {RowsClues}, 
 * the N column in {grid} is satisfied if it is equivalent in its Clue representation to the N Clue of {ColumnsClues}.
 * @see lineToClue.
*/
getClueStates(Grid, RowsClues, ColumnsClues, RowsCluesStates, ColumnsCluesStates) :-
	getAmountRows(Grid, AmountRows),
	getAmountColumns(Grid, AmountColumns),
	findall(RowSatisfied,
		(
			between(0, AmountRows, RowIdx),
			getRow(Grid, RowIdx, Row),
			getListElement(RowsClues, RowIdx, RowClue),
			(
				(lineToClue(Row, RowClue),
			     RowSatisfied = 1)
				;
				RowSatisfied = 0
			)
		), RowsCluesStates
	),
	findall(ColumnSatisfied,
		(
			between(0, AmountColumns, ColumnIdx),
			getColumn(Grid, ColumnIdx, Column),
			getListElement(ColumnsClues, ColumnIdx, ColumnClue),
			(
				(lineToClue(Column, ColumnClue),
			     ColumnSatisfied = 1)
				;
				ColumnSatisfied = 0
			)
		), ColumnsCluesStates
	).