import React from 'react';

function Square({ cursorModifier, stateValue, onMouseDown, onMouseUp, onMouseEnter, onContextMenu }) {
    let stateModifier = stateValue === '#' ? " square-painted" : stateValue==='X' ? " square-cross" : "";
    const className = "square" + stateModifier + cursorModifier;
    
    return (
        <button className={className}
            onMouseDown={onMouseDown}
            onMouseEnter={onMouseEnter}
            onMouseUp={onMouseUp}
            onContextMenu={onContextMenu}>
        </button>
    );
}

export default Square;