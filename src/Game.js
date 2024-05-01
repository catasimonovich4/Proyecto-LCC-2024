import React, { useEffect, useState, useRef } from 'react';
import PengineClient from './PengineClient';
import Board from './Board';
import Button from './Button';

let pengine;

function Game() {

  // State
  const [grid, setGrid] = useState(null);
  const [rowsClues, setRowsClues] = useState(null);
  const [colsClues, setColsClues] = useState(null);
  const [waiting, setWaiting] = useState(false);
  const [drawState, setDrawState] = useState(false);
  
  const [isMouseDown, setIsMouseDown] = useState(false);
  
  const NUMBER_ROWS_REF = useRef(0);
  //const NUMBER_COLS_REF = useRef(0);
  const visitedSquaresRef = useRef(new Set());
  
  const [rowsCluesStates, setRowsCluesStates] = useState(null);
  const [colsCluesStates, setColsCluesStates] = useState(null);

  
  useEffect(() => {
    // Creation of the pengine server instance.    
    // This is executed just once, after the first render.    
    // The callback will run when the server is ready, and it stores the pengine instance in the pengine variable. 
    PengineClient.init(handleServerReady);
  }, []);
  
  function handleServerReady(instance) {
    pengine = instance;
    const queryS = 'init(RowClues, ColumClues, Grid)';
    pengine.query(queryS, (success, response) => {
      if (success) {
        setGrid(response['Grid']);
        setRowsClues(response['RowClues']);
        setColsClues(response['ColumClues']);
        
        let amountRows = response['RowClues'].length;
        let amountCols = response['ColumClues'].length;
        NUMBER_ROWS_REF.content = amountRows;
        //NUMBER_COLS_REF.content = amountCols;
        
        setRowsCluesStates(Array(amountRows).fill(0));
        setColsCluesStates(Array(amountCols).fill(0));
      }
    });
  }
  
  // must rename.
  function getDrawState() { return drawState ? 'X' : '#' }

  function updateSquare(content, row, column) {
    // No action on click if we are waiting.
    if (waiting) { return; }
    
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

        if (rowSatisfied !== rowsCluesStates[row]) {
          const temp = [...rowsCluesStates];
          temp[row] = rowSatisfied;
          setRowsCluesStates(temp);
        }
        if (colSatisfied !== colsCluesStates[column]) {
          const temp = [...colsCluesStates];
          temp[column] = colSatisfied;
          setColsCluesStates(temp);
        }
      }
      setWaiting(false);
    });
  }


  function handleMouseDown(e, row, column) {
    let squareId = row * NUMBER_ROWS_REF.content + column;
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
    if (e.button === 0 || e.button === 2) { // left mouse button.
      setIsMouseDown(false);
      visitedSquaresRef.current.clear();
    }
  }
  
  function handleMouseEnter(row, column) {
    let squareId = row * NUMBER_ROWS_REF.content + column;
    if (isMouseDown && !visitedSquaresRef.current.has(squareId)) {
      let content = getDrawState();
      updateSquare(content, row, column);
      visitedSquaresRef.current.add(squareId);
    }
  }

  function handleRightClick(e) { e.preventDefault(); }
  

  if (!grid) {
    return null;
  }


  const won = rowsCluesStates.every( (e) => e===1 ) && colsCluesStates.every( (e) => e===1 );
  const statusText = won ? "WOK!" : "Keep playing!";
  return (
    <div className="game">
      <h1>Nonograma</h1>
      <Board
        grid={grid}
        rowsClues={rowsClues}
        colsClues={colsClues}
        rowsCluesStates={rowsCluesStates}
        colsCluesStates={colsCluesStates}
        onMouseDown={handleMouseDown}
        onMouseUp={handleMouseUp}
        onMouseEnter={handleMouseEnter}
        onContextMenu={handleRightClick}
      />
      <div className="game-info">
        {statusText}
      </div>
      <div className="switch-button">
        <Button value="#" onClick={() => setDrawState(!drawState)}></Button>
      </div>
    </div>
  );
}

export default Game;