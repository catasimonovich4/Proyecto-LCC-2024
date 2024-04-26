import { useState } from "react";


function Button({ onClick }) {
    let [state, setState] = useState(null);

    return ( 
            <label className="switch slider">
                <input value={state} type="checkbox" onClick={onClick}/>
            </label>
         );
}


export default Button;