
function Button({ onClick, state, children }) {
    let _className = state ? "button-80" : "button-80 buttonactive"
    return (
        <button className={_className} type="button" role="button" onClick={onClick}>{children}</button>
     ); 
}

export default Button;