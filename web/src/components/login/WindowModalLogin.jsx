import React from 'react';
import { Modal, Button, Form } from "react-bootstrap";
import { LoginFacade } from "../../facade/LoginFacade";
import {StringUtil} from "../../util/StringUtil";
import {AlmacenFacade} from '../../store/AlmacenFacade';

/**
 * Componente que muestra la ventana modal de login que permite la autenticación de los usuarios
 * @author <a href="mailto:oscar.rodriguezbrea@gmail.com">Óscar Rodríguez</a>
 */
class WindowModalLogin extends React.Component {

    /**
     * Constructor
     * @param {Properties} props 
     */
    constructor(props) {
        super(props);
        this.handleClose = this.handleClose.bind(this);
        this.handleAceptar  = this.handleAceptar.bind(this);
        this.pulsarTecla = this.pulsarTecla.bind(this);
        this.state =  { showModal: false,cerrar:false };
        // Se crea una referencia al div que representa el área de mensajes de error
        this.areaMensajeError = React.createRef();
        // Se crea una referencia a la caja de texto en la que el usuario introducirá el login
        this.login = React.createRef();
        // Se crea una referencia a la caja de texto en la que el usuario introducirá el password
        this.password = React.createRef();
    }

    /**
     * Método componentDidMount
     */
    componentDidMount() {

    }

    /**
     * Método componentDidUnmount
     */
    componentWillUnmount() {

    }

    /**
     * Método invocado cuando se actualiza el estado del componente
     * @param {prevProps} prevProps 
     */
    componentDidUpdate(prevProps) {
        this.state.showModal = false;
        this.state.cerrar = false;
        
    }


    /**
     * Método que despache la acción que permite autenticar un usuario
     * @param {Object} usuario: Objeto con los datos del usuario autenticado
     * recuperados del servior
     */
    despacharAccionAutenticacion(usuario) {
        AlmacenFacade.dispatchAutenticarUsuarioAction(usuario);
    }


    /**
     * Método invocado cuando el usuario pulsa el botón Cancelar
     */
    handleClose() { 
        this.setState({
            showModal: false,
            cerrar: true
        });
    }

    /**
     * 
     * @param {*} evt 
     */
    handleAceptar(evt) { 
        evt.preventDefault();
        var login = this.login.current.value;
        var password = this.password.current.value;

        if(StringUtil.isNotEmpty(login) && StringUtil.isNotEmpty(password)) {
            LoginFacade.authenticate(login,password)
            .then(resultado=>{
                switch(resultado.status){
                    case 0: {
                        // Autenticación correcta, se cierra la ventana modal y hay que modificar
                        // el estado general de la aplicación a autenticado
                        this.despacharAccionAutenticacion(resultado.user);
                        // Se informa al menú de que tiene que actualizarse tras la autentación del usuario
                        this.props.actualizarMenu();

                        this.setState({
                            showModal: false,
                            cerrar: true
                        });

                        // Se hace una redirección al listado de álbumes del usuario
                        window.location.href="/pr_albumes";
                        break;
                    }

                    case 1: {
                        this.mostrarMensajeError(this.getDescripcionMensajeError(1));
                        break;
                    }

                    case 2: {
                        this.mostrarMensajeError(this.getDescripcionMensajeError(2));
                        break;
                    }

                    case 3: {
                        this.mostrarMensajeError(this.getDescripcionMensajeError(3));
                        break;
                    }

                    default: {
                        break;
                    }
                }// switch

            }).catch(err=>{                
                console.log("error = " + err.message);
                this.mostrarMensajeError(this.getDescripcionMensajeError(1));
            });
        }
    }

    /**
     * Muestra un mensaje en el área dedicada a mensajes de error
     * @param {String} msg  Mensaje de error
     */
    mostrarMensajeError(msg) {
        if(StringUtil.isNotEmpty(msg)) {
            this.areaMensajeError.current.innerHTML = msg;
        }
    }

    /**
     * Limpia el área de mensajes de error de cualquier contenido
     */
    limpiarMensajeError() {
        this.areaMensajeError.current.innerHTML = "";
    }

    pulsarTecla(evt) {
        this.limpiarMensajeError();
    }

    /**
     * Comprueba si en el estado del componente se ha definido un mensaje de error dentro del atributo tipo_mensaje_error.
     * En ese caso, devuelve la descripción asociada 
     * al tipo de error
     */
    getDescripcionMensajeError(numero) {
        var salida;
        if(numero!==undefined && numero!==undefined) {
            switch(numero) {
                case 1: {
                    salida = "Se ha producido un error durante la autenticación. Intentelo de nuevo";
                    break;
                }

                case 2: {
                    salida = "No existe una cuenta de usuario con los datos introducidos";
                    break;
                }

                case 3: {
                    salida = "La cuenta de usuario indicada está desactivada";
                    break;
                }

                default: {
                    salida = null;
                    break;
                }
            }// switch
        }
        return salida;
    }

    /**
     * Método que renderiza la vista    
     */
    render() { 
        let mostrar = (!this.state.showModal && !this.state.cerrar);
        var estiloMsgError = {
            color:'red',
            fontSize:'15px',
            textAlign:'center'
        }
        
        return (
            <div>
                <Modal show={mostrar} onHide={this.handleClose} animation={false}>
                    <Modal.Header>
                        <Modal.Title>Autenticación</Modal.Title>
                    </Modal.Header>

                    <Form onSubmit={this.handleAceptar}>
                        <Modal.Body>
                            <Form.Group controlId="formGridLogin">
                                <div className="form-group">
                                    <label htmlFor="login"><span id="nombreUsuario">Nombre de usuario</span>:</label>
                                    <div className="col-sm-15">
                                        <input type="text" ref={this.login} className="form-control" size="50" id="username" name="username" placeholder="Nombre de usuario" required="required" autoFocus onKeyDown={this.pulsarTecla}/>
                                    </div>
                                </div>
                            </Form.Group>

                            <Form.Group controlId="formGridPassword">
                                <div className="form-group">
                                    <label htmlFor="login"><span id="passwordUsuario">Contraseña</span>:</label>
                                    <div className="col-sm-15">
                                        <input type="password" ref={this.password} className="form-control" id="password" name="password" placeholder="Contraseña" required="required" onKeyDown={this.pulsarTecla}/>
                                    </div>
                                </div>
                            </Form.Group>

                            <div ref={this.areaMensajeError} style={estiloMsgError}/>
                        </Modal.Body>

                        <Modal.Footer>
                            <Button variant="primary" type="submit">
                                Aceptar
                            </Button>

                            <Button variant="secondary" onClick={this.handleClose}>
                                Cancelar
                            </Button>
                        </Modal.Footer>
                    </Form>
                </Modal>
            </div>
        );
    }
};

export default WindowModalLogin;