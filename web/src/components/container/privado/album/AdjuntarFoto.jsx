import React from 'react';
import { Button} from "react-bootstrap";
import ReactDOM from 'react-dom';
import Form from 'react-bootstrap/Form';
import Breadcrumb from 'react-bootstrap/Breadcrumb'
import ComponenteAutenticado from '../autenticacion/ComponenteAutenticado';
import ErrorMessage from '../../../error/ErrorMessage';
import FileList from '../../common/Filelist';
import ModalProgressBar from '../../common/ModalProgressBar';
import AreaMensajes from '../../../mensajes/AreaMensajes';
import FileUtil from '../../../../util/FileUtil';
import {AlbumFacade} from '../../../../facade/AlbumFacade';
import {AlmacenFacade} from '../../../../store/AlmacenFacade';

/**
 * Componente a través del cual se puede adjuntar fotografías a una álbum
 * 
 * @author <a href="mailto:oscar.rodriguezbrea@gmail.com">Óscar Rodríguez</a>
 */
class AdjuntarFoto extends ComponenteAutenticado {

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
        window.location.href="/pr_albumes";
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
            });

            this.evaluarFicheros(evt.target.files);
        }
    }


    /**
     * Se evaluan los ficheros subidos al servidor para detectar si se deshabilita
     * el envio de archivos al backend
     * @param {Array} ficheros subidos al servidor
     */
    evaluarFicheros(ficheros) {
        var correcto = true;
        
        for(var i=0;i<ficheros.length;i++) {
            let fichero = ficheros[i];
            // Se comprueba si el fichero tiene un tipo mime válido
            if(!FileUtil.verificarTipoMime("imagen",fichero)) {
                correcto = false;
            }
        }

        if(!correcto) {
            this.botonAceptar.current.disabled = true;       
           
            this.setState({
                mostrarAreaMensajes: true,
                mensajeAreaMensajes: 'Entre la selección de archivos realizados hay algunos que tienen un formato de fotografía no válida. Realiza una nueva selección',
                tipoAreaMensajes: 'alert alert-danger'
            });

        }
        
    }



    /**
     * Método onClickFile que se ejecuta cuando se pulsa el boton [Buscar] para seleccionar un archivo
     * @param {Event} evt 
     */
    onClickFile(evt) {
        this.setState({
            mostrarAreaMensajes: false,
            mostrarListaFicheros:false
        });
        
        this.botonAceptar.current.disabled = true;        
    }s


    /**
     * Envio del vídeo al servidors
     * @param {Event} evt 
     */
    onSubmitFile(evt) {
        
        const ficheros = document.getElementById('fichero').files;

        // Cambios en el fichero
        //this.verifyImageFormat(ficheros);
        this.showProgressBar();

        AlbumFacade.submitFotos(this.props.match.params.p_album_id,AlmacenFacade.getUser(),ficheros)
        .then(resultado=>{       
            
            console.log("resultado = " + JSON.stringify(resultado));

            this.hideProgressBar();
            switch(resultado.status) {
                case 0: {
                    this.setState({
                        mostrarAreaMensajes: true,
                        mostrarListaFicheros:false,
                        mensajeAreaMensajes:"Se ha subido el vídeo correctamente",
                        tipoAreaMensajes: "success"
                    });
                    break;
                }

                case 1: {
                    this.setState({
                        mostrarAreaMensajes: true,
                        mensajeAreaMensajes:"Ya un vídeo con el mismo nombre en el servidor. Modifica el nombre y subelo de nuevo",
                        tipoAreaMensajes: "error"
                    });
                    break;
                }

                default: {
                    break;
                }
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
     * Muestra una modal que contiene una barra de prograso
     */
    showProgressBar() {
        // Se renderiza el componente WindowModalLogin indicando en la propiedad que se debe mostrar
        ReactDOM.render(<ModalProgressBar mensaje="Cargando fotografias" show={true}/>, document.getElementById('ventanaModal'));
    }

    /**
     * Oculta la barra de progreso abierta
     */
    hideProgressBar() {
        // Se renderiza el componente WindowModalLogin indicando en la propiedad que se debe mostrar
        ReactDOM.render(<ModalProgressBar mensaje="Cargando fotografias" show={false}/>, document.getElementById('ventanaModal'));
    }


      /**
     * Función invocada desde el componente FileList cuando este se actualice
     */
    executeOnErrorActionAfterUploadingVideo() {    
        this.botonAceptar.current.disabled = true;        
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
                    <h2>Adjuntar fotografías</h2>
                        <hr></hr>
                    </div>

                    <Breadcrumb>
                        <Breadcrumb.Item href="/">Inicio</Breadcrumb.Item>
                        <Breadcrumb.Item href="/pr_albumes">
                            Álbumes
                        </Breadcrumb.Item>
                        <Breadcrumb.Item href={'/pr_album/' + this.props.match.params.p_album_id}> 
                            Detalle           
                        </Breadcrumb.Item>                        
                        <Breadcrumb.Item href={'/pr_album/edit/' + this.props.match.params.p_album_id}>  
                            Edición                                    
                        </Breadcrumb.Item>
                        <Breadcrumb.Item active>Adjuntar fotografías</Breadcrumb.Item>
                    </Breadcrumb>

                   
                    <div className="row d-flex">
                        <div className="center">
                                       
                        <Form method="POST" role="form" action="" encType="multipart/form-data" multiple noValidate>
                        <div className="mb-3">
                            
            
                                <Form.Group controlId="formGridNombre">
                                    <div className="form-group">
                                        
                                        <div className="col-sm-16">
                                            
                                        <Form.File 
                                            id="fichero"
                                            label="Seleccionar fotografias"
                                            data-browse="Buscar"
                                            lang="es"
                                            custom               
                                            multiple={true}                             
                                            onChange={this.onChangeFile}
                                            onClick={this.onClickFile}
                                        />
                                        </div>    
                                    </div>
                                </Form.Group>
                        </div>
                        
                        
                        <FileList ficheros={this.state.ficheros} verificacionFormato="imagen" mostrar={this.state.mostrarListaFicheros} accion={this.executeOnErrorActionAfterUploadingVideo}/>     

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

export default AdjuntarFoto;