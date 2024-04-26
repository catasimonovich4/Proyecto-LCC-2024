import React from 'react';

function Square({ value, onMouseDown, onMouseUp, onMouseEnter, onContextMenu }) {
    return (
        <button className="square"
            onMouseDown={onMouseDown}
            onMouseEnter={onMouseEnter}
            onMouseUp={onMouseUp}
            onContextMenu={onContextMenu}>
            {value !== '_' ? value : null}
        </button>
    );
}

export default Square;