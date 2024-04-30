
function Button({ onClick }) {
    return ( 
            <label className="switch slider">
                <input type="checkbox" onClick={onClick}/>
                <span className="slider"></span>
            </label>
         );
}


export default Button;