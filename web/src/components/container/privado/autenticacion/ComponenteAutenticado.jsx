import React from 'react';
import {AlmacenFacade} from '../../../../store/AlmacenFacade';

/**
 * Clase de la que deben extender todos los componentes que requieran de autenticación.
 * Dispone de un mçetodo estaUsuarioAutenticado que devuelve un booleano que indica si el usuario está
 * o no autenticado
 * 
 * @author <a href="mailto:oscar.rodriguezbrea@gmail.com">Óscar Rodríguez</a>
 */
class ComponenteAutenticado extends React.Component{
    
    /**
     * Comprueba si el usuario que accede a este componente está autorizado, sino lo 
     * está se modifica el estado del componente para mostrar un mensaje de error
     */
    estaUsuarioAutenticado() {
        let salida = true;
        if(AlmacenFacade.getUser()===null || AlmacenFacade.getUser()===undefined) {
            salida = false;
        }
        return salida;
    }


   /**
     * Comprueba si el usuario está autenticado o si hay algun error definido
     * en el estado del componente (atributo "error" del state)
     * @return True si hay error y false en caso contrario
     */
    hayErrores() {
        var exito = false;

        if(!this.estaUsuarioAutenticado() || this.state.error===true) {
            exito = true;
        }
        return exito;
    }



    /**
     * Comprueba si hay que mostrar algún error al usuario para renderizar
     * el componente de error.
     * 
     * @return Devuelve un null o undefined sino hay error. En caso contrario devuelve
     * el texto del error a mostrar al usuario
     */
    getMensajeError() {
        var mensajeError;
        if(!this.estaUsuarioAutenticado()) {
            mensajeError = this.getEtiquetaTextoUsuarioNoAutenticado();
        }else 
        if(this.state.error===true){
            mensajeError = this.state.descError;
        }

        return mensajeError;
    }

    /**
     * Devuelve el texto a mostrar en el caso de que el usuario no esté autenticado
     */
    getEtiquetaTextoUsuarioNoAutenticado() {
        return "Para acceder a esta pantalla es necesario estar autenticado";
    }

} 

export default ComponenteAutenticado;