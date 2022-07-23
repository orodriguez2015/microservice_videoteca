import React from 'react';
import { Button, Form} from "react-bootstrap";
import {VideotecasFacade} from '../../../../facade/VideotecasFacade';
import {StringUtil} from '../../../../util/StringUtil';
import ErrorMessage from '../../../error/ErrorMessage';
import ComponenteAutenticado from '../autenticacion/ComponenteAutenticado';
import Breadcrumb from 'react-bootstrap/Breadcrumb';

/**
 * Componente VideotecaForm que contiene un formulario a través del cual se puede crear una nueva videoteca
 * @author <a href="mailto:oscar.rodriguezbrea@gmail.com">Óscar Rodríguez</a>
 */
class VideotecaForm extends ComponenteAutenticado {

    /**
     * Constructor
     * @param {Properties} props 
     */
    constructor(props) {
        super(props);
        this.nombre           = React.createRef();
        this.carpeta          = React.createRef();
        this.areaMensajeError = React.createRef();
        this.publico          = React.createRef();
        this.handleAceptar    = this.handleAceptar.bind(this);
        this.pulsarTecla      = this.pulsarTecla.bind(this);
        this.handleChangeNombre = this.handleChangeNombre.bind(this);
        this.handleChangeCarpeta = this.handleChangeCarpeta.bind(this);

        this.state = {
            error: false,
            descripcionError:''
        }
    }


    /**
     * Manejador asociado al botón [Aceptar]
     * @param {Event} evt 
     */
    handleAceptar(evt) { 
        evt.preventDefault();
        
        VideotecasFacade.comprobarRutaExistenciaVideoteca(this.carpeta.current.value)
        .then(resultado =>{
            console.log("resu = " + JSON.stringify(resultado));

            console.log("status = " + resultado.status);
            if(resultado.status === "OK" && resultado.data===false) {  
                console.log("palante")
                this.guardarVideoteca();
            }else {
                this.mostrarMensajeError("La carpeta ya existe en disco y está asociada a otra videoteca. Prueba con otro nombre e intentelo de nuevo");
            }

        }).catch(err=>{
            this.mostrarMensajeError("Se ha producido un error al comprobar si la carpeta ya existe en disco");
        });

    }


    /**
     * Método/Función que se invoca cuando se procede a guardar una videoteca
     */
    guardarVideoteca() {
        //let user = AlmacenFacade.getUser();

        VideotecasFacade.save(this.nombre.current.value,this.carpeta.current.value,this.publico.current.checked)
        .then(resultado=>{
            switch(resultado.status) {
                case 0: {
                    // Se redirige a la pantalla de administración de videotecas
                    this.props.history.push('/pr_videotecas');
                    break;
                }
                case 1: {
                    this.mostrarMensajeError("Se ha producido un error al guardar la videoteca. Intentelo de nuevo");
                    break;
                }

                default: {
                    this.mostrarMensajeError("Se ha producido un error al guardar la videoteca. Intentelo de nuevo");
                    break;
                }
            }
        }).catch(err=>{
            this.mostrarMensajeError("Se ha producido un error genérico al dar de alta la videoteca. Intentelo de nuevo");
        });         
    }


    /**
     * Manejador asociado al botón "Cerrar"
     * @param {Event} evt 
     */
    handleClose(evt) {
        window.location.href="/pr_videotecas"
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

    /**
     * Método que se invoca cuando es pulsada una tecla. La acción que 
     * se desencadena es la de borrar el área de mensajes de error
     */
    pulsarTecla() {
        this.limpiarMensajeError();
    }


    /**
     * Manejador para el evento change asociado al campo de formulario "Nombre"
     * @param {Event} evt 
     */
    handleChangeNombre(evt) {
        evt.preventDefault();
        this.limpiarMensajeError();
    }

    /**
     * Manejador para el evento change asociado al campo de formulario "Carpeta"
     * @param {Event} evt 
     */
    handleChangeCarpeta(evt) {
        evt.preventDefault();
        this.limpiarMensajeError();
    }

    /**
     * Método que renderiza la vista
     */
    render() {
        var estiloMsgError = {
            color:'red',
            fontSize:'15px',
            textAlign:'center'
        }

        if(this.hayErrores()===true) {
            return (<ErrorMessage mensaje={this.getMensajeError()}/>);   
        } else {

            return (

                <div className="container">
                    <div className="subtitulo">
                        <h2>Alta de una nueva videoteca</h2>
                        <hr></hr>
                    </div>


                    <Breadcrumb>
                        <Breadcrumb.Item href="/">Inicio</Breadcrumb.Item>
                        <Breadcrumb.Item href="/pr_videotecas">
                            Videotecas
                        </Breadcrumb.Item>          

                        <Breadcrumb.Item active>Alta</Breadcrumb.Item>
                    </Breadcrumb>




                    <div className="row d-flex">
                        <div className="center">
                    
                        <Form onSubmit={this.handleAceptar}>
                                
                            <Form.Group controlId="formGridNombre">
                                <div className="form-group">
                                    <Form.Label align="left">Nombre *</Form.Label>
                                    <div className="col-sm-10">
                                        <input type="text" ref={this.nombre} className="form-control" size="50" id="nombre" name="nombre" placeholder="Nombre de la videoteca" required="required" onChange={this.handleChangeNombre} autoFocus/>
                                    </div>    
                                </div>
                            </Form.Group>

                            <Form.Group controlId="formGridPassword">
                                <div className="form-group">
                                    <label htmlFor="login"><span id="Descripcion">Carpeta en disco que contiene los vídeos *</span>:</label>
                                    <div className="col-sm-10">
                                        <input type="text" ref={this.carpeta} className="form-control" id="carpeta" name="carpeta" placeholder="Carpeta" required="required" onChange={this.handleChangeCarpeta}/>
                                    </div>
                                </div>
                            </Form.Group>

                            <Form.Group controlId="formBasicCheckbox">
                                <Form.Check type="checkbox" ref={this.publico} label="Público" />
                            </Form.Group>

                            
                        
                            <div align="right">
                                <Button variant="success" type="submit">
                                    Aceptar
                                </Button>
                                &nbsp;&nbsp;

                                <Button variant="danger" onClick={this.handleClose}>
                                    Cancelar
                                </Button>
                            </div>                    
                        </Form> 
                    
                    </div>
                </div>
                <p></p>
                <div ref={this.areaMensajeError} style={estiloMsgError}/>
                </div>
            );
        }
    }
}

export default VideotecaForm;