import React, { useState } from 'react';

function ToggleButton({ onClick, children }) {
    const [isActive, setIsActive] = useState(false);

    const handleClick = () => {
        setIsActive(!isActive);
        onClick();
    };

    const buttonClass = isActive ? 'button-80 active-show-solution' : 'button-80 inactive-show-solution';

    return (
        <button
            className={buttonClass}
            id="toggleButton"
            type="button"
            role="button"
            onClick={handleClick}
        >
            {children}
        </button>
     ); 
}

export default ToggleButton;