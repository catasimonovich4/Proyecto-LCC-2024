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
  const visitedSquaresRef = useRef(new Set());

  
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
        NUMBER_ROWS_REF.content = response['Grid'].length;
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
      if (success) { setGrid(response['ResGrid']); }
      setWaiting(false);
    });
  }


  function handleMouseDown(e, row, column) {
    // left mouse button.
    let squareId = row * NUMBER_ROWS_REF.content + column;
    if (e.button === 0) {
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
    // left mouse button.
    if (e.button === 0 || e.button === 2) {
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

  const statusText = 'Keep playing!';
  return (
    <div className="game">
      <Board
        grid={grid}
        rowsClues={rowsClues}
        colsClues={colsClues}
        onMouseDown={handleMouseDown}
        onMouseUp={handleMouseUp}
        onMouseEnter={handleMouseEnter}
        onContextMenu={handleRightClick}
      />
      <div className="game-info">
        {statusText}
      </div>
      <div>
        <Button value="#" onClick={() => setDrawState(!drawState)}></Button>
      </div>
    </div>
  );
}

export default Game;