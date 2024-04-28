
function Button({ onClick }) {
    return ( 
            <label class="switch" className="switch slider">
                <input type="checkbox" onClick={onClick}/>
                <span class="slider"></span>
            </label>
         );
}


export default Button;