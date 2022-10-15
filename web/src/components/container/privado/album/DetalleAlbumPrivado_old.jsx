import React from 'react';
import {AlbumFacade} from '../../../../facade/AlbumFacade';
import {StringUtil} from '../../../../util/StringUtil';
import ErrorMessage from '../../../error/ErrorMessage';
import {URL_BACKEND_IMAGES} from '../../../../constantes/Configuracion';
import {ESTADO_PUBLICACION_FOTO,ESTADO_DESPUBLICACION_FOTO} from '../../../../constantes/Constantes';
import SimpleReactLightbox from "simple-react-lightbox"; 
import { SRLWrapper} from 'simple-react-lightbox'; 
import ComponenteAutenticado from '../autenticacion/ComponenteAutenticado';
import ModalConfirmation from '../../../modal/ModalConfirmation';
import { AlmacenFacade } from '../../../../store/AlmacenFacade';
import Breadcrumb from 'react-bootstrap/Breadcrumb';
import ReactDOM from 'react-dom';

/**
 * Componente DetalleAlbumPrivado que muestra las fotografías de un determinado álbum.
 * También permite realizar la administración del álbum fotográfico
 * 
 * @author <a href="mailto:oscar.rodriguezbrea@gmail.com">Óscar Rodríguez</a>
 */
class DetalleAlbumPrivado extends ComponenteAutenticado {

    /**
     * Constructor
     * @param {Properties} props 
     */
    constructor(props) {
        super(props);
        this.state = {
            fotos :[],
            nombreAlbum:'',
            error: false,
            descError:''
        }

        this.onConfirmEliminarFoto = this.onConfirmEliminarFoto.bind(this);
        this.onConfirmPublicarFoto = this.onConfirmPublicarFoto.bind(this);
        this.areaMensajeError = React.createRef();
    }

    
    /**
     * Método componentDidMount, desde que que se recuperan los álbumes públicos
     */
    componentDidMount() { 
        if(this.estaUsuarioAutenticado()) {

            // Se recupera el id del álbum de la url
            var idAlbum = this.props.match.params.p_album_id;
    
            if(this.props.match.params===null || this.props.match.params.p_album_id===undefined) {
                // Se comprueba si está definido el state en la llamada a este componente que llega a través de un "a href", como 
                // no lo está habrá que mostrar un error
                this.setState({
                    fotos:[],
                    nombreAlbum:'',
                    error:true,
                    descError: 'Álbum desconocido'
                });
            } else {
                // Se recupera el id del álbum pasado como parámetro
                this.reloadPhotos(idAlbum);

            }
        }
    }
    


    /**
     * Manejador para el borrado de una foto
     * @param {Integer} Id de la fotografía
     */
    handleEliminar(id) {
        var mensaje ="¿ Deseas eliminar la fotografía con id #" + id + " ?";
        this.borrarMensajeError();
        // Se renderiza el componente WindowModalLogin indicando en la propiedad que se debe mostrar
        ReactDOM.render(<ModalConfirmation show={true} title="Atención" onConfirm={()=>this.onConfirmEliminarFoto(id)} message={mensaje} textButtonConfirm="Aceptar" textButtonCancel="Cancelar"/>, document.getElementById('ventanaModal'));
    }



    /**
     * Recarga las fotos del álbum
     * @param {Integer} idAlbum 
     */
    reloadPhotos(idAlbum) {
        if(StringUtil.isNotEmpty(idAlbum)) { 

            let user = AlmacenFacade.getUser();
            AlbumFacade.getFotosAlbumPrivado(idAlbum,user)
            .then(resultado=>{

                if(resultado.status==="OK") {
                    this.setState({
                        fotos: resultado.data.fotos,
                        nombreAlbum: resultado.data.nombre
                    });
                }

            }).catch(err=>{
                this.mostrarMensajeError("Se ha producido un error al recuperar las fotografías del álbum");
            });
            
        } else {
            this.mostrarMensajeError("Se ha producido un error al recuperar el álbum");
        }
     }


    /**
     * Envío de la petición de borrado de la fotografía al servidor
     * @param {Integer} Id de la fotografía 
     */
    onConfirmEliminarFoto(id) {
        AlbumFacade.deleteFoto(id,AlmacenFacade.getUser())
        .then(resultado=>{
            
            if(resultado.status==="OK"){
               this.reloadPhotos(this.props.match.params.p_album_id);
            } else {
                this.mostrarMensajeError("Se ha producido un error técnico al eliminar la fotografía");
            }
                    
        }).catch(error=>{
            this.mostrarMensajeError("Se ha producido un error genérico al eliminar la fotografía");
        });
    }


   /**
     * Devuelve la etiqueta de texto que se muestra a la hora de publicar/despublicar un vídeo
     * @param {Integer} id 
     */
    getEtiquetaTextoConfirmacionPublicacion(id) {
        let mensaje = "";
        const key = id + "_publico";

        if(id!==undefined && document.getElementById(key).value!==undefined) {
            mensaje = (document.getElementById(key).value===ESTADO_PUBLICACION_FOTO)?"¿Deseas despublicar la foto con id #" + id + "?":"¿Deseas publicar la foto con id #" + id + "?";
        } 
        return mensaje;
    }


   /**
     * Manejador para ocultar/mostrar una foto
     * @param {Integer} Id de la fotografía
     */
    handleOcultarFoto(id) {
        const mensaje = this.getEtiquetaTextoConfirmacionPublicacion(id);
        this.borrarMensajeError();
        // Se renderiza el componente WindowModalLogin indicando en la propiedad que se debe mostrar
        ReactDOM.render(<ModalConfirmation show={true} title="Atención" onConfirm={()=>this.onConfirmPublicarFoto(id)} message={mensaje} textButtonConfirm="Aceptar" textButtonCancel="Cancelar"/>, document.getElementById('ventanaModal'));
    }


    /**
     * Se envia petición al servidor
     * @param {Integer} id 
     */
    onConfirmPublicarFoto(id) {
        let key = id + "_publico";
        let keyImage = id + "_image";
        var value = document.getElementById(key).value;

        AlbumFacade.publicarFoto(id,AlmacenFacade.getUser().id,this.getValorPublicar(value),AlmacenFacade.getUser())
        .then(resultado=>{
        
            switch(resultado.codStatus) {
                case 200: {
                    // Actualizar imagen y estado de visibilidad de la fotografía
                    document.getElementById(key).value = (value===ESTADO_PUBLICACION_FOTO)?ESTADO_DESPUBLICACION_FOTO:ESTADO_PUBLICACION_FOTO;
                    document.getElementById(keyImage).src   = this.getImagenPublicar(document.getElementById(key).value);
                    break;
                }

                default: {
                    this.mostrarMensajeError("Se ha producido un error técnico al publicar/despublicar la fotografía");
                    break;
                }

            }

        }).catch(error=>{
            this.mostrarMensajeError("Se ha producido un error técnico al publicar/despublicar la fotografía");
        });
        
    }


     /**
     * Devuelve la imagen correspondiente al valor del parámetro publico
     * @param {String} publico Si valor='0' => se devuelve '/images/ojo_cerrado.png' y si tiene valor=1 => Se devuelve '/images/ojo_abierto.png'
     * @return Integer
     */
    getImagenPublicar(publico) {
        let salida = "/images/ojo_cerrado.png";

        if(publico!==undefined && publico!==null && publico===ESTADO_PUBLICACION_FOTO) {
            salida = "/images/ojo_abierto.png";
        }

        return salida;
    }


    /**
     * Devuelve el valor distinto al valor del parámetro público.
     * @param {String} publico Si valor='true' => se devuelve '0' y si tiene valor=false => Se devuelve 1
     * @return Integer
     */
    getValorPublicar(publico) {
        let salida = ESTADO_PUBLICACION_FOTO;
        
        if(publico!==undefined && publico!==null && publico===ESTADO_PUBLICACION_FOTO) {
            salida = ESTADO_DESPUBLICACION_FOTO;
        }
        return salida;
    }

   /**
     * Muestra un mensaje en el área dedicada a mensajes de error
     * @param {String} msg  Mensaje de error
     */
    mostrarMensajeError(msg) {
        if(StringUtil.isNotEmpty(msg)) {
            this.areaMensajeError.current.innerHTML = msg;

            // Dirigir el scroll a la parte superior de la página
            window.scrollTo({
                top: 0, 
                behavior: 'auto'
                /* you can also use 'auto' behaviour
                   in place of 'smooth' */
              });
        }
    }


    /**
     * Borrar mensaje de error
     */
    borrarMensajeError() {
        this.areaMensajeError.current.innerHTML = "";
    }

    /**
     * Método que renderiza la vista
     */
    render() { 
        if(this.hayErrores()===true) {
            // Sino se ha comunicado un idAlbum => Entonces se muestra la pantalla de error
            return (
                <ErrorMessage mensaje={this.getMensajeError()}/>
            );
        } else {

            // const options = {
            //     settings: {
            //         overlayColor: "rgb(0, 0, 0.9)",
            //         showCaption: false,
            //         buttonsBackgroundColor: "rgba(0, 0, 0, 0.9)",   
            //         buttonsIconColor: "rgba(219, 219, 219, 0.7)",
            //         showThumbnails: true,
            //         showControls: true,
            //         transitionSpeed: 200,
            //         transitionTimingFunction: "linear"
            //     }
            // };


            const options = {
                settings: {
                  autoplaySpeed: 8000,
                  hideControlsAfter: 3000,
                  disableKeyboardControls: false,
                  disableWheelControls: false,
                  disablePanzoom: true,
                  lightboxTransitionSpeed: 300,
                  lightboxTransitionTimingFunction: 'ease',
                  overlayColor: 'rgba(0, 0, 0, 0.9)',
                  slideTransitionSpeed: 100,
                  slideTransitionTimingFunction: 'ease'
                //   transitionSpeed: 100,
                //   transitionTimingFunction: "linear"
                },

                thumbnails: {
                    showThumbnails: false,
                    thumbnailsOpacity: 0.4,
                    thumbnailsSize: ['100px', '80px']
                  }
            };

            return (
                
                <div className="container">
                    <div className="subtitulo">
                        <h2>Fotografías del álbum {this.state.nombreAlbum}</h2>
                        <hr></hr>
                    </div>


                    <Breadcrumb>
                        <Breadcrumb.Item href="/">Inicio</Breadcrumb.Item>
                        <Breadcrumb.Item href="/pr_albumes">
                            Álbumes
                        </Breadcrumb.Item>                                                
                        <Breadcrumb.Item href={'/pr_album/edit/' + this.props.match.params.p_album_id}> 
                            Edición              
                        </Breadcrumb.Item>

                        <Breadcrumb.Item href={'/pr_album/adjuntar/' + this.props.match.params.p_album_id}> 
                            Adjuntar fotografías                                   
                        </Breadcrumb.Item>              

                        <Breadcrumb.Item active>Detalle</Breadcrumb.Item>
                    </Breadcrumb>


                    <div className="row">
                        <div ref={this.areaMensajeError} className="mensajeError"/>
                    </div>
                
                    <SimpleReactLightbox>
                    <SRLWrapper options={options}>

                    <div className="row">
                        {this.state.fotos.map((value, index) => {
                            // Se construye la ruta de la miniatura en el servidor
                        
                            //let imgOriginal  = URL_BACKEND_IMAGES + value.rutaRelativa;
                            let imgOriginal  = require(URL_BACKEND_IMAGES + value.rutaRelativa);

                            
                            

                            let imagen = value.publico===1?'/images/ojo_abierto.png':'/images/ojo_cerrado.png';
                            let key = value.id + "_publico";
                            let keyImage = value.id + "_image";
                            
                            return <div key={value.id} className="col-3">
                    



                                    <a href={`${imgOriginal}`} data-attribute="SRL">
                                        <img src={`${imgOriginal}`} id={`${imgOriginal}`} alt={`${imgOriginal}`} srl_gallery_image='true' width="200" height="150"/>
                                    </a>                
                                    
                                    <p className="nombreVideoFoto">{value.nombre}</p>                        
                                    <p className="idVideoFoto">{value.descripcion}</p>                                        
                                    <p className="idVideoFoto">ID # {value.id}</p>
                                    <p className="idVideoFoto">Visto { value.numeroVisualizaciones } veces</p>
                                    <p className="idVideoFoto">Alta el {value.fechaAltaFormato}</p>            
                                                    
                                    <img src={imagen} id={keyImage} name={keyImage} border="0" width="26" height="26" title="Mostrar/Ocultar" alt="Mostrar/Ocultar" onClick={()=>this.handleOcultarFoto(value.id)}/>
                                    <img src="/images/full_trash.png"  border="0" width="20" height="20" title="Eliminar" alt="Eliminar" onClick={()=>this.handleEliminar(value.id)}/>
                                    <input type="hidden" id={key} name={key} value={value.publico}/>
                                    <p></p>
                                </div>
                        })}
                    </div>
                    </SRLWrapper>
                    </SimpleReactLightbox>
                    
                </div>
                
              );
        }
    }
}
export default DetalleAlbumPrivado;