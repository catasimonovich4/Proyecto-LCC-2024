import React from 'react';

function Square({ value, onMouseDown, onMouseUp, onMouseEnter, onContextMenu }) {
    let stateModifier = value === '#' ? " square-painted" : value==='X' ? " square-cross" : "";
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