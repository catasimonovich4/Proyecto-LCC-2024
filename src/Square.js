import React from 'react';

function Square({ stateValue, onMouseDown, onMouseUp, onMouseEnter, onContextMenu }) {
    let stateModifier = stateValue === '#' ? " square-painted" : stateValue==='X' ? " square-cross" : "";
    const className = "square" + stateModifier;
    
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