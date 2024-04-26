
function Button({ onClick }) {
    return ( 
            <label className="switch slider">
                <input type="checkbox" onClick={onClick}/>
            </label>
         );
}


export default Button;