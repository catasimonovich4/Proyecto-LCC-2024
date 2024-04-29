import React from 'react';

function Clue({ clue, clueState }) {

    let stateModifier = clueState ? "clue-active" : "clue-inactive";

    let className = "clue " + stateModifier;
    return (
        <div className={className} >
            {clue.map((num, i) =>
                <div key={i}>
                    {num}
                </div>
            )}
        </div>
    );
}



export default Clue;