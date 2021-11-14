import React from 'react';
import { Button, Form} from "react-bootstrap";
import {AlbumFacade} from '../../../../facade/AlbumFacade';
import {AlmacenFacade} from '../../../../store/AlmacenFacade';
import {StringUtil} from '../../../../util/StringUtil';
import ErrorMessage from '../../../error/ErrorMessage';
import ComponenteAutenticado from '../autenticacion/ComponenteAutenticado';
import Breadcrumb from 'react-bootstrap/Breadcrumb';


/**
 * Componente AlbumForm que contiene un formulario a través del cual se puede crear un nuevo álbum, 
 * una vez que el usuario se ha autenticado
 * @author <a href="mailto:oscar.rodriguezbrea@gmail.com">Óscar Rodríguez</a>
 */
class AlbumForm extends ComponenteAutenticado {

    /**
     * Constructor
     * @param {Properties} props 
     */
    constructor(props) {
        super(props);
        this.nombre           = React.createRef();
        this.descripcion      = React.createRef();
        this.areaMensajeError = React.createRef();
        this.publico          = React.createRef();
        this.handleAceptar    = this.handleAceptar.bind(this);
        this.pulsarTecla      = this.pulsarTecla.bind(this);

        this.state = {
            error:false,
            descError:''
        }
    }

   
    /**
     * Manejador asociado al botón [Aceptar]
     * @param {Event} evt 
     */
    handleAceptar(evt) {
        evt.preventDefault();
        let user = AlmacenFacade.getUser();
       
        AlbumFacade.saveAlbum(this.nombre.current.value,this.descripcion.current.value,this.publico.current.checked,user.id)
        .then(resultado=>{
            switch(resultado.status) {
                case 0: {
                    // Se redirige a la pantalla de administración de álbumes
                    window.location.href="/pr_albumes";
                    break;
                }
                case 1: {
                    this.mostrarMensajeError("Se ha producido un error al guardar el álbum. Intentelo de nuevo");
                    break;
                }

                default: {
                    this.mostrarMensajeError("Se ha producido un error al guardar el álbum. Intentelo de nuevo");
                    break;
                }
            }
        }).catch(err=>{
            this.mostrarMensajeError("Se ha producido un error genérico al dar de alta el álbum fotográfico. Intentelo de nuevo");
        });        
    }


    /**
     * Manejador asociado al botón "Cerrar"
     * @param {Event} evt 
     */
    handleClose(evt) {
        evt.preventDefault();
        window.location.href="/pr_albumes"
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
     * Método que renderiza la vista
     */
    render() {
        var estiloMsgError = {
            color:'red',
            fontSize:'15px',
            textAlign:'center'
        }

        
        if(this.hayErrores()===true) {
            return (<ErrorMessage mensaje={this.getMensajeError()}/>)
        } else {

            return (
                <div className="container">
                    <div className="subtitulo">
                        <h2>Alta de un álbum fotográfico</h2>
                        <hr></hr>
                    </div>


                    <Breadcrumb>
                        <Breadcrumb.Item href="/">Inicio</Breadcrumb.Item>
                        <Breadcrumb.Item href="/pr_albumes">
                            Álbumes
                        </Breadcrumb.Item>          

                        <Breadcrumb.Item active>Alta</Breadcrumb.Item>
                    </Breadcrumb>


                    <div className="row d-flex">
                        <div className="center">
                    
                        <Form onSubmit={this.handleAceptar}>
                                
                            <Form.Group controlId="formGridNombre">
                                <div className="form-group">
                                    <Form.Label align="left">Nombre:</Form.Label>
                                    <div className="col-sm-10">
                                        <input type="text" ref={this.nombre} className="form-control" size="50" id="nombre" name="nombre" placeholder="Nombre del álbum" required="required" autoFocus/>
                                    </div>    
                                </div>
                            </Form.Group>

                            <Form.Group controlId="formGridPassword">
                                <div className="form-group">
                                    <label htmlFor="login"><span id="Descripcion">Descripción</span>:</label>
                                    <div className="col-sm-10">
                                        <input type="text" ref={this.descripcion} className="form-control" id="descripcion" name="descripcion" placeholder="Descripción del álbum" required="required"/>
                                    </div>
                                </div>
                            </Form.Group>

                            <Form.Group controlId="formBasicCheckbox">
                                <Form.Check type="checkbox" ref={this.publico} label="Público" />
                            </Form.Group>

                            <div ref={this.areaMensajeError} style={estiloMsgError}/>
                        
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
                </div>
            );
        }
    }
}

export default AlbumForm;