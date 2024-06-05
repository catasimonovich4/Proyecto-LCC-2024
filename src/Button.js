
function Button({ onClick, children }) {
    return (
        <button className="button-80" type="button" role="button" onClick={onClick}>{children}</button>
     ); 
}

export default Button;