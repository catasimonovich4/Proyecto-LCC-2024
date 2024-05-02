import React from "react";

const GameWon = ({ onButtonClick }) => {
  return (
    <div className="winning-screen">
      <h1>YOU WON!</h1>
      <button onClick={onButtonClick}>Play Again</button>
    </div>
  );
};

export default GameWon;