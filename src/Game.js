import React, { useEffect, useState, useRef } from 'react';
import PengineClient from './PengineClient';
import Board from './Board';
import SliderButton from './SliderButton';
import Button from './Button';

let pengine;

function Game() {

  // State
  const [solvedGrid, setSolvedGrid] = useState(null);
  const [grid, setGrid] = useState(null);
  const [rowsClues, setRowsClues] = useState(null);
  const [colsClues, setColsClues] = useState(null);
  const [waiting, setWaiting] = useState(false);
  const [drawState, setDrawState] = useState(false);
  
  const [isMouseDown, setIsMouseDown] = useState(false);
  
  const visitedSquaresRef = useRef(new Set());
  const gridPlaceholder = useRef(null);
  
  const [rowsCluesStates, setRowsCluesStates] = useState(null);
  const [colsCluesStates, setColsCluesStates] = useState(null);
  
  const [userWon, setUserWon] = useState(false);
  const [showSolution, setShowSolution] = useState(true);
  const [canPlay, setCanPlay] = useState(false);
  const [nextClickHint, setNextClickHint] = useState(false);
  
  useEffect(() => {
    // Creation of the pengine server instance.    
    // This is executed just once, after the first render.    
    // The callback will run when the server is ready, and it stores the pengine instance in the pengine variable. 
    PengineClient.init(handleServerReady);
  }, []);
  
  function handleServerReady(instance) {
    pengine = instance;
    const queryS = "init(RowClues, ColumnClues, Grid), " +
    "getClueStates(Grid, RowClues, ColumnClues, RowsCluesStates, ColumnsCluesStates), " +
    "solveGrid(Grid, RowClues, ColumnClues, SolvedGrid)";
    
    pengine.query(queryS, (success, response) => {
      if (success) {
        gridPlaceholder.current = response['Grid'];
        setGrid(response['Grid']);
        setRowsClues(response['RowClues']);
        setColsClues(response['ColumnClues']);
        setRowsCluesStates(response['RowsCluesStates']);
        setColsCluesStates(response['ColumnsCluesStates']);
        setUserWon(response['RowsCluesStates'].every( (e) => e===1 ) && response['ColumnsCluesStates'].every( (e) => e===1 ));
        setSolvedGrid(response['SolvedGrid']);
        setCanPlay(true);
      }
    });
  }
  
  // must rename.
  function getDrawState() { return drawState ? 'X' : '#' }

  function updateSquare(content, row, column) {
    // No action on click if we are waiting or user can not play.
    if (waiting || !canPlay) { return; }

    if (nextClickHint) {
      setNextClickHint(false);

      // if already correct do not modify.
      if (grid[row][column] === solvedGrid[row][column]) { return; }

      content = solvedGrid[row][column];
    } 
    // Build Prolog query to make a move and get the new satisfacion status of the relevant clues.    
    const squaresS = JSON.stringify(grid).replaceAll('"_"', '_'); // Remove quotes for variables. squares = [["X",_,_,_,_],["X",_,"X",_,_],["X",_,_,_,_],["#","#","#",_,_],[_,_,"#","#","#"]]
    const rowsCluesS = JSON.stringify(rowsClues);
    const colsCluesS = JSON.stringify(colsClues);
    const queryS = `put("${content}", [${row},${column}], ${rowsCluesS}, ${colsCluesS}, ${squaresS}, ResGrid, RowSat, ColSat)`; // queryS = put("#",[0,1],[], [],[["X",_,_,_,_],["X",_,"X",_,_],["X",_,_,_,_],["#","#","#",_,_],[_,_,"#","#","#"]], GrillaRes, FilaSat, ColSat)
    setWaiting(true);
    pengine.query(queryS, (success, response) => {
      if (success) {
        setGrid(response['ResGrid']);
        let rowSatisfied = response['RowSat'];
        let colSatisfied = response['ColSat'];

        const rowClues = [...rowsCluesStates];
        const columnClues = [...colsCluesStates];

        if (rowSatisfied !== rowsCluesStates[row]) {
          rowClues[row] = rowSatisfied;
          setRowsCluesStates(rowClues);
        }
        if (colSatisfied !== colsCluesStates[column]) {
          columnClues[column] = colSatisfied;
          setColsCluesStates(columnClues);
        }

        let tempWon = rowClues.every( (e) => e===1 ) && columnClues.every( (e) => e===1 );
        if (tempWon) {
            setCanPlay(false);
            setUserWon(true);
        }
      }

      setWaiting(false);
    });
  }

  function handleMouseDown(e, row, column) {
    e.preventDefault();
    let squareId = `${row}${column}`;
    if (e.button === 0) { // left mouse button.
      setIsMouseDown(true);
      
      let content = getDrawState();
      updateSquare(content, row, column);
      visitedSquaresRef.current.add(squareId);
    }
    else if (e.button === 2) { // mouse right button.
      let content = "_";
      updateSquare(content, row, column);
      visitedSquaresRef.current.add(squareId);
    }
  }
  
  function handleMouseUp(e) {
    e.preventDefault();
    if (e.button === 0 || e.button === 2) { // left mouse button.
      setIsMouseDown(false);
      visitedSquaresRef.current.clear();
    }
  }
  
  function handleMouseEnter(row, column) {
    let squareId = `${row}${column}`;
    if (isMouseDown && !visitedSquaresRef.current.has(squareId)) {
      let content = getDrawState();
      updateSquare(content, row, column);
      visitedSquaresRef.current.add(squareId);
    }
  }
  
  function handleRightClick(e) { e.preventDefault(); }
  
  function peekSolution() {
    if (showSolution) {
      gridPlaceholder.current = grid;
      setGrid(solvedGrid);
      setCanPlay(false);
    } else {
      setGrid(gridPlaceholder.current);
      setCanPlay(!userWon);
    }
    setShowSolution(!showSolution); 
  }
  
  
  if (!grid) { return null; }

  const statusText = userWon ? "YOU WON!" : "Keep playing!";
  const cursorModifier = (nextClickHint && !userWon) ? " custom-cursor" : "";
  return (
    <div className="game">
      <h1>Nonograma</h1>
      <Board
        grid={grid}
        rowsClues={rowsClues}
        colsClues={colsClues}
        rowsCluesStates={rowsCluesStates}
        colsCluesStates={colsCluesStates}
        cursorModifier={cursorModifier}
        onMouseDown={handleMouseDown}
        onMouseUp={handleMouseUp}
        onMouseEnter={handleMouseEnter}
        onContextMenu={handleRightClick}
        />
      
      <div className="game-info">
        {statusText}
      </div>

      <Button onClick={() => {setNextClickHint(!nextClickHint);}}>Revelar Celda</Button>
      <Button onClick={peekSolution}>Mostrar Solucion</Button>

      <div className="switch-button">
        <SliderButton currentState={drawState} onClick={() => setDrawState(!drawState)}></SliderButton>
      </div>

    </div>
  );
}

export default Game;