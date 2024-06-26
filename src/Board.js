import React from 'react';
import Square from './Square';
import Clue from './Clue';

function Board({ grid, rowsClues, colsClues, rowsCluesStates, colsCluesStates, cursorModifier, onMouseDown, onMouseUp, onMouseEnter, onContextMenu }) {
    const numOfRows = grid.length;
    const numOfCols = grid[0].length;


    
    let maxRowClue = Math.max(...rowsClues.map(clue => clue.length));
    let maxColClue = Math.max(...colsClues.map(clue => clue.length));

    let rowClueWidth = 16 + maxRowClue*13;
    let colClueWidth = 15 + maxColClue*23;

    return (
        <div className="vertical">
            <div
                className="colClues"
                style={{
                    gridTemplateRows: `${colClueWidth}px`,
                    gridTemplateColumns: `${rowClueWidth}px repeat(${numOfCols}, 40px)`
                    /*
                       60px  40px 40px 40px 40px 40px 40px 40px   (gridTemplateColumns)
                      ______ ____ ____ ____ ____ ____ ____ ____
                     |      |    |    |    |    |    |    |    |  60px
                     |      |    |    |    |    |    |    |    |  (gridTemplateRows)
                      ------ ---- ---- ---- ---- ---- ---- ---- 
                     */
                }}
            >
                <div>{/* top-left corner square */}</div>
                {colsClues.map((clue, i) =>
                    <Clue clue={clue} clueState={colsCluesStates[i]} key={i} />
                )}
            </div>
            <div className="horizontal">
                <div
                    className="rowClues"
                    style={{
                        gridTemplateRows: `repeat(${numOfRows}, 40px)`,
                        gridTemplateColumns: `${rowClueWidth}px`
                        /* IDEM column clues above */
                    }}
                >
                    {rowsClues.map((clue, i) =>
                        <Clue clue={clue} clueState={rowsCluesStates[i]} key={i} />
                    )}
                </div>
                <div className="board" onMouseLeave={onMouseUp}
                    style={{
                        gridTemplateRows: `repeat(${numOfRows}, 40px)`,
                        gridTemplateColumns: `repeat(${numOfCols}, 40px)`
                    }}>
                    {grid.map((row, i) =>
                        row.map((cell, j) =>
                            <Square
                                cursorModifier={cursorModifier}
                                stateValue={cell}
                                onMouseDown={(e) => onMouseDown(e, i, j)}
                                onMouseUp={onMouseUp}
                                onMouseEnter={() => onMouseEnter(i, j)}
                                onContextMenu={(e) => onContextMenu(e, i, j)}
                                key={i + j}
                            />
                        )
                    )}
                </div>
            </div>
        </div>
    );
}

export default Board;