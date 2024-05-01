
function Button({ currentState, onClick }) {
    return (
           /*<label className="switch slider">
                <input type="checkbox" onClick={onClick}/>
                <span className="slider"></span>
            </label>*/
            <div className="div-button">
                <div className="checkbox-wrapper-22">
                    <label className="switch" type="checkbox">
                        <input type="checkbox" id="checkbox" onClick={onClick}/>
                        <div className="slider round"></div>
                    </label>
                </div>
            </div>
         );
}


export default Button;