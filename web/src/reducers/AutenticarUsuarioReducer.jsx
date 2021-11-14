import {ESTADO_USUARIO_INICIAL} from '../constantes/Constantes';

/**
 * Reducer llamado cuando un usuario se ha autenticado o cierra su sesión de usuario
 * @param {Estado} state: Estado inicial que es un objeto que no tiene los datos del usuario autenticado
 * @param {Action} action: Acción de tipo AutenticarUsuarioAction que será la que contenga
 * el estado al que se pretende cambiar, en ese caso, los datos del usuario una vez autenticado
 */
function autenticarUsuarioReducer(state = ESTADO_USUARIO_INICIAL, action) {
    switch (action.type) {
        case 'AUTENTICAR_USUARIO': {
            let newState = JSON.parse(JSON.stringify(state));
            newState = action.usuario;
            // Se almacena los datos del usuario en el SessionStorage
            sessionStorage.setItem("usuario",JSON.stringify(action.usuario));
            return newState;
        }
  
        case 'CERRAR_SESION_USUARIO': {
            // Se eliminan los datos del usuario en el SessionStorage
            sessionStorage.removeItem("usuario");
            // Se devuelve los datos vacíos del objeto usuario, porque se ha cerrado la sesión
            return ESTADO_USUARIO_INICIAL;
        }

        default: {
            return state;
        }
    }
}
export default autenticarUsuarioReducer;
