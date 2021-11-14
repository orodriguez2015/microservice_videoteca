import React from 'react';
import {StringUtil}  from '../../util/StringUtil';
import {MENSAJE_ERROR_GENERICO} from '../../constantes/Mensajes';

/**
 * Componente ErrorMessage encargada de mostrar un mensaje de error al usuario.
 * Este mensaje de error puede ser genérico o se puede pasar un mensaje personalizado a través
 * de las propiedades del componente
 * 
 * Para pasar el mensaje a mostrar, se pasa en las props del componente con el nombre mensaje
 * 
 * @author <a href="mailto:oscar.rodriguezbrea@gmail.com">Óscar Rodríguez</a>
 */
class ErrorMessage extends React.Component{

    /**
     * Método que renderiza la vista para mostrar un mensaje de error al usuario
     */
    render() {
       let mensajeError = (StringUtil.isNotEmpty(this.props.mensaje))?this.props.mensaje:MENSAJE_ERROR_GENERICO;
        return (
            <div align="center" className="cabeceraMensajeError">
                {mensajeError}
            </div>
        );

    }
}

export default ErrorMessage;