import React, { useEffect, useState, useRef } from 'react';
import PengineClient from './PengineClient';
import Board from './Board';
import Button from './Button';
import GameWon from './GameWon';
let pengine;

function Game() {

  // State
  const [grid, setGrid] = useState(null);
  const [rowsClues, setRowsClues] = useState(null);
  const [colsClues, setColsClues] = useState(null);
  const [waiting, setWaiting] = useState(false);
  const [drawState, setDrawState] = useState(false);
  
  const [isMouseDown, setIsMouseDown] = useState(false);

  const [win, setWin] = useState(false);

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
    const queryS = 'init(RowClues, ColumnClues, Grid), getClueStates(Grid, RowClues, ColumnClues, RowsCluesStates, ColumnsCluesStates)';
    pengine.query(queryS, (success, response) => {
      if (success) {
        setGrid(response['Grid']);
        setRowsClues(response['RowClues']);
        setColsClues(response['ColumnClues']);
        setRowsCluesStates(response['RowsCluesStates']);
        setColsCluesStates(response['ColumnsCluesStates']);
        setWin(response['RowsCluesStates'].every( (e) => e===1 ) && response['ColumnsCluesStates'].every( (e) => e===1 ));
      }
    });
  }

  function restartGame() {
    const queryS = 'init(RowClues, ColumnClues, Grid), getClueStates(Grid, RowClues, ColumnClues, RowsCluesStates, ColumnsCluesStates)';
    pengine.query(queryS, (success, response) => {
      if (success) {
        setGrid(response['Grid']);
        setRowsClues(response['RowClues']);
        setColsClues(response['ColumnClues']);
        setRowsCluesStates(response['RowsCluesStates']);
        setColsCluesStates(response['ColumnsCluesStates']);
        setWin(response['RowsCluesStates'].every( (e) => e===1 ) && response['ColumnsCluesStates'].every( (e) => e===1 ));
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

        setWin(rowClues.every( (e) => e===1 ) && columnClues.every( (e) => e===1 ));

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

  if (!grid) {
    return null;
  }

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

      <div className="switch-button">
        <Button currentState={drawState} onClick={() => setDrawState(!drawState)}></Button>
      </div>

      <div className="game-info">
        <div className="game-container">
          {win ? <GameWon onButtonClick={() => restartGame(pengine)} /> : (<div className="game-info"> "Keep playing!" </div>)}
        </div>
      </div>

    </div>
  );
}

export default Game;