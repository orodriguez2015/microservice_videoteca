import React from 'react';
import { Button} from "react-bootstrap";
import ReactDOM from 'react-dom';
import Form from 'react-bootstrap/Form';
import Breadcrumb from 'react-bootstrap/Breadcrumb'
import ComponenteAutenticado from '../autenticacion/ComponenteAutenticado';
import ErrorMessage from '../../../error/ErrorMessage';
import FileList from '../../common/Filelist';
import ModalProgressBar from '../../common/ModalProgressBar';
import {VideotecasFacade} from '../../../../facade/VideotecasFacade';
import AreaMensajes from '../../../mensajes/AreaMensajes';
import {INTERNAL_SERVER_ERROR,HTTP_OK} from '../../../../constantes/HttpResponse';

/**
 * Componente a través del cual se puede adjuntar un vídeo a una videoteca
 * en particular
 * 
 * @author <a href="mailto:oscar.rodriguezbrea@gmail.com">Óscar Rodríguez</a>
 */
class AdjuntarVideo extends ComponenteAutenticado {

    /**
     * Constructor
     * @param {Properties} props 
     */
    constructor(props) {
        super(props);
        this.botonAceptar = React.createRef();
        this.areaMensajesError = React.createRef();
        this.onChangeFile = this.onChangeFile.bind(this);
        this.onSubmitFile = this.onSubmitFile.bind(this);
        this.onClickFile = this.onClickFile.bind(this);
        this.executeOnErrorActionAfterUploadingVideo = this.executeOnErrorActionAfterUploadingVideo.bind(this);
        
        this.state = {
            ficheros: [],
            mostrarListaFicheros: false,
            mostrarAreaMensajes: false,
            mensajeAreaMensajes:"",
            tipoAreaMensajes:"error"
        }
    }


    /**
     * Función asociada al botón [Cancelar]
     */
    handleClose() {
        window.location.href="/pr_videotecas";
    }


    /**
     * Método que se invoca cuando se ha seleccionado un fichero
     * @param {Event} evt 
     */
    onChangeFile(evt) { 
        evt.preventDefault();    
        if(evt.target.files!==undefined) {                
            this.botonAceptar.current.disabled = false;
            this.setState({
                ficheros: evt.target.files,
                mostrarListaFicheros: true
            })
        }
    }

    /**
     * Método onClickFile que se ejecuta cuando se pulsa el boton [Buscar] para seleccionar un archivo
     * @param {Event} evt 
     */
    onClickFile(evt) {
    //       this.setState({
    //           mostrarAreaMensajes: false,
    //           mostrarListaFicheros:false
    //      });

        this.hideErrorMessageArea(false,false);
        this.botonAceptar.current.disabled = true;
    }

    /**
     * Oculta el área de mensajes de error
     */
    hideErrorMessageArea(showMessageArea,showFileList) {
        this.setState({
            mostrarAreaMensajes:showMessageArea,
            mostrarListaFicheros:showFileList
        })
    }

    /**
     * Envio del vídeo al servidor
     * @param {Event} evt 
     */
    onSubmitFile(evt) {
        const fichero = document.getElementById('fichero').files;

        this.showProgressBar();
        this.hideErrorMessageArea(false,true);
        
        VideotecasFacade.submitVideo(this.props.match.params.p_videoteca_id,fichero)
        .then(resultado=>{       
            
            this.hideProgressBar();

            console.log("resultado = " + JSON.stringify(resultado));
            if(resultado.codStatus===HTTP_OK) {
                
                    this.setState({
                        mostrarAreaMensajes: true,
                        mostrarListaFicheros:false,
                        mensajeAreaMensajes:"Se ha subido el vídeo correctamente",
                        tipoAreaMensajes: "success"
                    });
            } else
            if(resultado.codStatus===INTERNAL_SERVER_ERROR) {

                    this.setState({
                        mostrarAreaMensajes: true,
                        mensajeAreaMensajes:"Se ha producido un error al subir el vídeo al servidor. Inténtalo de nuevo.",
                        tipoAreaMensajes: "error"
                    });
            }

        }).catch(err=>{
            this.setState({
                mostrarAreaMensajes: true,
                mensajeAreaMensajes:"Se ha producido un error al subir el vídeo al servidor. Inténtalo de nuevo.",
                tipoAreaMensajes: "error"
            });
        });
    } 


    /**
     * Función invocada desde el componente FileList cuando este se actualice
     */
    executeOnErrorActionAfterUploadingVideo() {        
        this.botonAceptar.current.disabled = true;        
    }


    /**
     * Muestra una modal que contiene una barra de prograso
     */
    showProgressBar() {
        // Se renderiza el componente WindowModalLogin indicando en la propiedad que se debe mostrar
        ReactDOM.render(<ModalProgressBar mensaje="Cargando vídeo" show={true}/>, document.getElementById('ventanaModal'));
    }

    /**
     * Oculta la barra de progreso abierta
     */
    hideProgressBar() {
        // Se renderiza el componente WindowModalLogin indicando en la propiedad que se debe mostrar
        ReactDOM.render(<ModalProgressBar mensaje="Cargando vídeo" show={false}/>, document.getElementById('ventanaModal'));
    }

    /**
     * Método encargado de renderizar la vista
     */
    render() {
        if(!this.estaUsuarioAutenticado()) {
            return (<ErrorMessage mensaje={this.getEtiquetaTextoUsuarioNoAutenticado()}/>);   
            
        } else {
            return (
 
                <div className="container">                                
                    <div className="subtitulo">
                    <h2>Adjuntar vídeos {this.props.match.params.p_videoteca_id}</h2>
                        <hr></hr>
                    </div>

                    <Breadcrumb>
                        <Breadcrumb.Item href="/">Inicio</Breadcrumb.Item>
                        <Breadcrumb.Item href="/pr_videotecas">
                            Videotecas
                        </Breadcrumb.Item>
                        <Breadcrumb.Item href={'/pr_videoteca/' + this.props.match.params.p_videoteca_id}> 
                            Detalle           
                        </Breadcrumb.Item>                        
                        <Breadcrumb.Item href={'/pr_videoteca/edit/' + this.props.match.params.p_videoteca_id}>  
                            Edición                                    
                        </Breadcrumb.Item>
                        <Breadcrumb.Item active>Adjuntar vídeo</Breadcrumb.Item>
                    </Breadcrumb>

                   
                    <div className="row d-flex">
                        <div className="center">
                                       
                        <Form method="POST" role="form" action="" encType="multipart/form-data"  noValidate>
                        <div className="mb-3">
                            
            
                                <Form.Group controlId="formGridNombre">
                                    <div className="form-group">
                                        
                                        <div className="col-sm-16">
                                            
                                        <Form.File 
                                            id="fichero"
                                            label="Selecciona un archivo de vídeo"
                                            data-browse="Buscar"
                                            lang="es"
                                            custom                                            
                                            onChange={this.onChangeFile}
                                            onClick={this.onClickFile}
                                        />
                                        </div>    
                                    </div>
                                </Form.Group>
                        </div>
                        
                        
                        <FileList ficheros={this.state.ficheros} verificacionFormato="video" mostrar={this.state.mostrarListaFicheros} accion={this.executeOnErrorActionAfterUploadingVideo}/>     
                        <AreaMensajes mostrar={this.state.mostrarAreaMensajes} mensaje={this.state.mensajeAreaMensajes} tipo={this.state.tipoAreaMensajes}/>
                    
                        <div align="right">
                            <Button variant="success" id="botonAceptar" ref={this.botonAceptar} onClick={this.onSubmitFile}>
                                Enviar
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

export default AdjuntarVideo;