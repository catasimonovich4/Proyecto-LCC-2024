:- module(proylcc, [put/8, getClueStates/5]).
:- use_module(library(lists)).
:- use_module(library(clpfd)).

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
	(lineToClue(NewRow, RowClue) -> RowSat = 1; RowSat = 0),
	(lineToClue(Column, ColClue) -> ColSat = 1; ColSat = 0).
	

/******************************************************************************** 
 * getListElement(+List, +Idx, -Element).
 * 
 * Succeeds if the {Idx} element of {List} is equal to {Element}.
**/
getListElement([X | _], 0, X) :- !.
getListElement([_ | Xs], Idx, Element) :-
	K is Idx-1,
	getListElement(Xs, K, Element).

getRow(Grid, RowIdx, Row) :- getListElement(Grid, RowIdx, Row).

getColumn(Grid, ColIdx, Col) :- maplist(nth0(ColIdx), Grid, Col).

getAmountRows(Grid, AmoutRows) :- length(Grid, AmoutRows).

getAmountColumns([X | _], AmountColumns) :- length(X, AmountColumns).

/******************************************************************************** 
 * skipConsecutiveSymbols(+List, +Symbol, -Count :int, -Sublist).
 * 
 * Succeeds if the first {Count} elements of {List} are equal to {Symbol},
 * and the sublist containing the remaining {length(List)-Count} elements is equal to {Sublist}.
**/
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
 * @param Clue is a list of ints(>0) representing a Line,
 * 			   such that every int is the amount of consecutive "#"s
 * 			   followed or preceded by any amount of other elements(_).
**/
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
**/
getClueStates(Grid, RowsClues, ColumnsClues, RowsCluesStates, ColumnsCluesStates) :-
	getAmountRows(Grid, AmountRows),
	getAmountColumns(Grid, AmountColumns),
	R is AmountRows-1,
	C is AmountColumns-1,
	findall(RowSatisfied,
		(
			between(0, R, RowIdx),
			getRow(Grid, RowIdx, Row),
			getListElement(RowsClues, RowIdx, RowClue),
			(lineToClue(Row, RowClue) -> RowSatisfied = 1 ; RowSatisfied = 0)
		), RowsCluesStates
	),
	findall(ColumnSatisfied,
		(
			between(0, C, ColumnIdx),
			getColumn(Grid, ColumnIdx, Column),
			getListElement(ColumnsClues, ColumnIdx, ColumnClue),
			(lineToClue(Column, ColumnClue) -> ColumnSatisfied = 1 ; ColumnSatisfied = 0)
		), ColumnsCluesStates
	).

/******************************************************************************** 
 * clueToLine(+Clue :list, -Line :list).
 * 
 * Line MUST BE A FIXED SIZE LIST.
 * Succeeds if the {Clue} representation can unify to the {Line} representation.
 * 
 * 
 * @param Line is a list which contains "#", "X" or anything else(_). @see getRow/3, getColumn/3.
 * @param Clue is a list of ints(>0) representing a Line,
 * 			   such that every int is the amount of consecutive "#"s
 * 			   followed or preceded by any amount of other elements(_).
 *
 * @example
 *   ?- clueToLine([2, 1, 3], Line).
 *   INFINITE RECURSION.
 * 
 *   ?- clueToLine([1, 2], [_,_,_,_]).
 *   true.
 * 
 *   ?- L=[_,_,_,_], clueToLine([1, 1], L).
 * 	 L = ["X", "#", "X", "#"] ;
 * 	 L = ["#", "X", "X", "#"] ;
 * 	 L = ["#", "X", "#", "X"] ;
 *   false.
 * 
 * safer to use solveLine/3.
**/
clueToLine([], []).
clueToLine(Clue, [X | Xs]) :-
	X = "X",
	clueToLine(Clue, Xs).
clueToLine([ Count | Counts ], List) :- 
	List = [ "#" | _ ],
	findall("#", between(1, Count, _), Hashtags), % Hashtags = lista con {Count} "#"s.
	append(Hashtags, SubList, List),
	(
		SubList = [];
		(SubList = [ First | _ ], First = "X")
	),
	clueToLine(Counts, SubList).

/******************************************************************************** 
 * solveLine(+Line, +Clue, -LineSolved).
 * 
 * Succeeds if
 *  - {LineSolved} unifies with {Line},
 *  - {LineSolved} only contains "#" and "X",
 *  - {LineSolved} is a valid representation of {Clue}.
 * 
 * given a semi-completed {Line} and a {Clue}, {LineSolved} will give all
 * possible solutions to the {Clue} with {Line} as the initial state.
 * 
 * @param Line is a list which contains "#", "X" or anything else(_).
 * @param Clue is a list of ints(>0) representing a Line,
 * 			   such that every int is the amount of consecutive "#"s
 * 			   followed or preceded by any amount of other elements(_).
 * @param LineSolved is a list containing "#" or "X", which corresponds to Clue representation.
 *
**/
solveLine(Line, Clue, LineSolved) :- 
	LineSolved = Line,
	clueToLine(Clue, LineSolved).

/******************************************************************************** 
 * sameElement(+Lists:List of lists, +Idx:int, -Element:any).
 * 
 * Succeeds if ALL Lists inside {Lists} have {Element} at index {Idx}.
 *
 * @param Lists is a lists of lists.
 * @param Idx is the index to check in the Lists of lists, starts at 1. @see nth1/3.
 * @param Element is the same element in all lists at the specified index.
**/
sameElement([First | Rest], Idx, Element) :-
	nth1(Idx, First, Element),
	forall(member(List, Rest), (nth1(Idx, List, E), Element == E)).

/******************************************************************************** 
 * commonElements(+Lists, -Result).
 * 
 * Succeeds if {Result} is the most restricted list which can unify with any list inside {Lists}.
 *
 * @param Lists is a lists of lists.
 * @param Result is the most restricted list which can unify with any list inside {Lists}.
 *
 * @example
    ?- commonElements([[1, 2, 3], [1, 2, 3], [1, 2, 3]], Result1).
    Result1 = [1, 2, 3].
    
    ?- commonElements([[1, 2, 3], [1, _, 3], [1, 2, 3]], Result2).
    Result2 = [1, _, 3].
    
    ?- commonElements([[_, _, _], ['X', 'X', 'X'], [_, _, _]], Result3).
    Result3 = [_, _, _].

    ?- commonElements([['#', _, '#'], [_, 'X', _], [_, _, 'X']], Result4).
	Result4 = [_, _, _].

    ?- commonElements([['#', 'X', '#'], ['#', 'X', '#'], ['#', 'X', '#']], Result5).
    Result5 = ['#', 'X', '#'].

    ?- commonElements([['#', 'X', '#'], ['#', _, '#'], ['#', 'X', '#']], Result6).
    Result6 = ['#', _, '#'].

    ?- commonElements([['#', 'X', '#'], [_, _, _], ['#', 'X', '#']], Result7).
    Result7 = [_, _, _].
**/
commonElements(Lists, Result) :-
	Lists = [First | _],
	length(First, Len),
	findall(Element,
		(
        	between(1, Len, Idx),
			(sameElement(Lists, Idx, Element) -> true ; Element = _)
    	),
		Result).

/******************************************************************************** 
 * progressLine(+Line, +Clue, -AdvancedLine).
 * 
 * Succeeds if {AdvancedLine} contains the maximum information {"X" and "#"} it can represent with the
 * initial state as {Line} and satisfies {Clues}, can contain anything else(_).
 * 
 * %%%%%%%%%%%% This predicate should fail if the initial state of the Line can not satisfy Clues.
 *
 * @param Line Line is a list which contains "#", "X" or anything else(_). Can be empty or already initialized e.g [_,_,"#"] or [_,_,_].
 * @param Clue is a list of ints(>0) representing a Line,
 * 			   such that every int is the amount of consecutive "#"s
 * 			   followed or preceded by any amount of other elements(_).
* @param AdvancedLine contains the maximum information {"X" and "#"} it can represent with the initial state as {Line} and {Clues}, can contain anything else(_).
*/
progressLine(Line, Clue, AdvancedLine) :-
	findall(LineSolved, solveLine(Line, Clue, LineSolved), LinesPermutations),
	commonElements(LinesPermutations, AdvancedLine).

/******************************************************************************** 
 * progressGrid(+Grid, +RowsClues, +ColumnsClues, -AdvancedGrid).
 * 
 * Succeeds if {AdvancedGrid} is the result of applying progressLine/3 to every Row and Column of {Grid}
 * with clues {RowsClues} and {ColumnClues} respectively.
**/
progressGrid(Grid, RowsClues, ColumnsClues, AdvancedGrid) :-
		getAmountRows(Grid, AmountRows),
		getAmountColumns(Grid, AmountColumns),
		R is AmountRows-1,
		C is AmountColumns-1,

		% advance maximum possible with rows.
		findall(AdvancedRow,
			(
				between(0, R, RowIdx),
				getRow(Grid, RowIdx, Row),
				getListElement(RowsClues, RowIdx, RowClue),
				progressLine(Row, RowClue, AdvancedRow)
			), AdvancedRows
		),
		AdvancedGrid = AdvancedRows,

		% advance maximum possible with columns using already modified rows.
		findall(AdvancedColumn,
			(
				between(0, C, ColumnIdx),
				getColumn(AdvancedGrid, ColumnIdx, Column),
				getListElement(ColumnsClues, ColumnIdx, ColumnClue),
				progressLine(Column, ColumnClue, AdvancedColumn)
			), AdvancedColumns
		),

		transpose(AdvancedColumns, TAdvancedColumns),
		AdvancedGrid = TAdvancedColumns.

/******************************************************************************** 
 * maximumProgressGrid(+Grid, +RowsClues, +ColumnsClues, -AdvancedGrid).
 * 
 * Succeeds if {MaxAdvancedGrid} is the result of applying progressGrid/3 to {Grid}
 * with data {RowsClues} and {ColumnsClues} up until no more progress can be reached.
**/
maximumProgressGrid(Grid, RowsClues, ColumnsClues, MaxAdvancedGrid) :-
	progressGrid(Grid, RowsClues, ColumnsClues, AdvancedGrid),
	(sameGridProgress(Grid, AdvancedGrid) -> MaxAdvancedGrid=AdvancedGrid ; maximumProgressGrid(AdvancedGrid, RowsClues, ColumnsClues, MaxAdvancedGrid)).

% NumElements is the number of "#" and "X" found in Grid : list of lists.
countGridElements(Grid, NumElements) :-
	flatten(Grid, FlatGrid),
	findall(E, (member(E, FlatGrid), (E=="#";E=="X")), SymbolList),
	length(SymbolList, NumElements).

sameGridProgress(Grid1, Grid2) :-
	countGridElements(Grid1, N),
	countGridElements(Grid2, N).

containsVar(List) :- member(V, List), var(V). 

/******************************************************************************** 
 * incompletedRow(+Grid, -IncompletedRow).
 * 
 * Succeeds if any row in {Grid} contains a variable and
 * {IncompletedRow} is the first row with a variable.
 * 
 * @param Grid is a list of lists.
 * @param IncompletedRow is the first row with a variable (topleft to bottomright).
**/
incompletedRow([Row | _], Row) :- containsVar(Row), !.
incompletedRow([_ | Rest], Row) :- incompletedRow(Rest, Row).


/******************************************************************************** 
 * completedGrid(+Grid).
 * 
 * Succeeds if there is no list in grid which contains a variable.
 * 
 * @param Grid is a list of lists.
**/
completedGrid(Grid) :- \+incompletedRow(Grid, _).

getMostRestrictedRow([Row | _], 0, Row).
	

/******************************************************************************** 
 * solveGrid(+Grid, +RowsClues, +ColumnsClues, -SolvedGrid).
 * 
 * Succeeds if {SolvedGrid} is a grid that satisfies {RowsClues} and {ColumnsClues}, and unifies with {Grid}.
 * 
**/
solveGrid(Grid, RowsClues, ColumnsClues, SolvedGrid) :-
	maximumProgressGrid(Grid, RowsClues, ColumnsClues, AdvancedGrid),
	(
		completedGrid(AdvancedGrid) -> SolvedGrid = AdvancedGrid ;
		(
			getMostRestrictedRow(AdvancedGrid, Idx, Row),
			getListElement(RowsClues, Idx, RowClue),
			solveLine(Row, RowClue, PossibleRow),
			replace(Row, Idx, PossibleRow, AdvancedGrid, PossibleGrid),
			solveGrid(PossibleGrid, RowsClues, ColumnsClues, SolvedGrid)
		)
	).