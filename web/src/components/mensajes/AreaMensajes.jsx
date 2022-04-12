import React from 'react';

/**
 * Componente AreaMensajes que muestra un div de Bootstrap en el que 
 * se muestra un mensaje
 * 
 * Propiedades a pasar:
 *   - tipo: Si toma el valor "error" se asocia el estilo alert-danger de bootstrap a la capa en la que se muestra el mensaje 
 *           Si toma el valor "success" se asocia el estilo alert-success de bootstrap a la capa en la que se muestra el mensaje 
 *           Si toma el valor "warning" se asocia el estilo alert-warning de bootstrap a la capa en la que se muestra el mensaje 
 * 
 *   - mensaje: mensaje a mostrar
 *   - mostrar: true si se muestra la capa con el mensaje o false para que no esté visible
 */
class AreaMensajes extends React.Component {
    

    /**
     * Devuelve el nombre de la clase CSS a utilizar. Por defecto devuelve el estilo "alert alert-success" de bootstrap
     * 
     */
    getClassName() {
        var className = "alert alert-success";
        if(this.props.tipo!==undefined && this.props.tipo==="error"){
            className = "alert alert-danger";
        }else
        if(this.props.tipo!==undefined && this.props.tipo==="warning"){
            className = "alert alert-warning";
        }else
        if(this.props.tipo!==undefined && this.props.tipo==="success"){
            className = "alert alert-success";
        }

        return className;
    }

    /**
     * Método render
     */
    render() {
        const className = this.getClassName();
        const mensaje = (this.props.mensaje!==undefined && this.props.mensaje.length>0)?this.props.mensaje:"";
        const mostrar = (this.props.mostrar!==undefined && this.props.mostrar===true)?true:false;
        const areaMensajesStyle = {
            display: (mostrar===true)?"block":"none"
        }

        return (
            <div className={className} ref={this.areaMensajes} role="alert" style={areaMensajesStyle}>
                {mensaje}
            </div>
        );

    }
}

export default AreaMensajes;