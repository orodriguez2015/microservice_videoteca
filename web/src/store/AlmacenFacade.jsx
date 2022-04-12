import {store} from './Almacen';
import {StringUtil} from '../util/StringUtil';
import {autenticarUsuarioAction} from '../actions/AutenticarUsuarioAction';
import {cerrarSesionUsuarioAction} from '../actions/CerrarSesionUsuarioAction';

/**
 * Clase que actúa de fachada para acceder al store, y por tanto consultar
 * en el estado global de la aplicación
 * 
 * @author <a href="mailto:oscar.rodriguezbrea@gmail.com">Óscar Rodríguez</a>
 */
export class AlmacenFacade {
    
     /**
     * Comprueba si el usuario actual está o no autenticado. Para ello se comprueba si
     * existen los datos del usuario en el SessionStorage
     * @return Boolean
     */
    static isUserAuthenticated() {
        let exito = false;
        let aux = sessionStorage.getItem("usuario") || '';

        if(StringUtil.isNotEmpty(aux)) {
            let user = JSON.parse(aux);
            if(user!==null && user!==undefined && user.id!==-1 && StringUtil.isNotEmpty(user.login) && StringUtil.isNotEmpty(user.nombre)) {
               exito = true;
            }
        }        
        return exito;
    }

    /**
     * Devuelve los datos del usuario autenticado que están alojados en la sessionStorage
     * @return {User} Objeto usuario
     */
    static getUser() {
        let aux = sessionStorage.getItem("usuario") || '';
        let user;

        if(StringUtil.isNotEmpty(aux)) {
            user = JSON.parse(aux);
        }        
        return user;
    }


    /**
     * Despacha el action AutenticarUsuarioAction para dar por autenticado un usuario
     * @param usuario Objeto con los datos del usuario recien autenticado
     */
    static dispatchAutenticarUsuarioAction(usuario) {
        store.dispatch(autenticarUsuarioAction(usuario));
    }


    /**
     * Despacha el action CerrarSesionUsuarioAction para cerrar la sesión del usuaro
     */
    static dispatchCerrarSesionUsuarioAction() {
        store.dispatch(cerrarSesionUsuarioAction())
    }
}