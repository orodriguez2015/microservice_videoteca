import React from 'react';
import ReactDOM from 'react-dom';
import {VideotecasFacade} from '../../../../facade/VideotecasFacade';
import {StringUtil} from '../../../../util/StringUtil';
import ErrorMessage from '../../../error/ErrorMessage';
import VisorVideo from '../../video/VisorVideo';
import ModalConfirmation from '../../../modal/ModalConfirmation';
import { AlmacenFacade } from '../../../../store/AlmacenFacade';
import ComponenteAutenticado from '../autenticacion/ComponenteAutenticado';
import Breadcrumb from 'react-bootstrap/Breadcrumb'; 

/**
 * Componente que permite visualizar los vídeos de una determinada videoteca y proceder a su administración
 * @author <a href="mailto:oscar.rodriguezbrea@gmail.com">Óscar Rodríguez Brea</a>
 */
class DetalleVideoteca extends ComponenteAutenticado {

    /**
     * Constructor
     * @param {Properties} props 
     */
    constructor(props) {
        super(props);
        this.state = {
            videos:[],
            nombreVideoteca:'',
            error: false,
            descError:'',
            showModalConfirmDelete:false,
            showModalConfirmPublish:false
        }

        this.areaMensajeError = React.createRef();
        this.handleEliminar  = this.handleEliminar.bind(this);
        this.onConfirmEliminarVideo = this.onConfirmEliminarVideo.bind(this);
        this.handleEliminar = this.handleEliminar.bind(this);
    }


    /**
     * Método componentDidMount, desde el que se recuperan los vídeos que conforman una videoteca
     */
    componentDidMount() { 

        if(this.estaUsuarioAutenticado()) {
            // Se comprueba si se ha pasado el parámetro p_videoteca_id en la url. 
            if(this.props.match.params===null || this.props.match.params.p_videoteca_id===undefined) {
                this.setEstado([],'',true,'Videoteca desconocida');

            } else {
                // Se recupera el id del videoteca pasado como parámetro en la url
                var idVideoteca = this.props.match.params.p_videoteca_id;    
                this.getVideos(idVideoteca);
            }
        }
    }


    /**
     * Recuperar los vídeos de una videoteca
     * @param {Integer} idVideoteca Identifidador de la videoteca
     */
    getVideos(idVideoteca) {
        if(StringUtil.isNotEmpty(idVideoteca)) { 
            VideotecasFacade.getVideosFromVideoteca(idVideoteca,2)
            .then(resultado=>{
                this.setEstado(resultado.data.videos,resultado.data.nombre,false,'');
            }).catch(err=>{
                this.setEstado([],'',true,'Se ha producido un error al recuperar los vídeos que conforman las videotecas');
            });
            
        } else {
            this.setEstado([],'',true,'Videoteca desconocida');
        }
    }


    /**
     * Método setEstado que permite modificar el estado del componente
     * @param {Array} Array con los videos 
     * @param {String} nombreVideoteca  Nombre de la videoteca
     * @param {Boolean} error True si hay error y false en caso contrario
     * @param {String} descripcionError Descripcion del error en caso de que lo hubiese
     */
    setEstado(videos,nombreVideoteca,error,descripcionError) {
        this.setState({
            videos: (videos!=null)?videos:[],
            nombreVideoteca: StringUtil.isNotEmpty(nombreVideoteca)?nombreVideoteca:'',
            error: error,
            descError: StringUtil.isNotEmpty(descripcionError)?descripcionError:'Videoteca desconocida'
        });
    }


    /**
     * Función para manejar el evento de eliminar una video de una videoteca
     * @param {Integer} id: Id del vídeo 
     */
    handleEliminar(id) {
        this.limpiarMensajeError();
        let mensaje = "¿Deseas eliminar el vídeo con id #" + id + "?";
        // Se renderiza el componente WindowModalLogin indicando en la propiedad que se debe mostrar
        ReactDOM.render(<ModalConfirmation show={true} title="Atención" onConfirm={()=>this.onConfirmEliminarVideo(id)} message={mensaje} textButtonConfirm="Aceptar" textButtonCancel="Cancelar"/>, document.getElementById('ventanaModal'));
    }


    /**
     * Método que se ejecuta el usuario pulsa el botoón
     * @param {id} Id del vídeo a eliminar 
     */
    onConfirmEliminarVideo(id) {
        var user = AlmacenFacade.getUser();

        VideotecasFacade.deleteVideo(id,user.id).then(resultado=>{
    
            if(resultado.codStatus===200) {
                this.getVideos(this.props.match.params.p_videoteca_id);
            }else {
                this.mostrarMensajeError("Se ha producido un error al eliminar el vídeo con id #" + id + ". Intentalo de nuevo");
            }

        }).catch(error=>{
            this.mostrarMensajeError("Se ha producido un error desconocido al eliminar el vídeo con id #" + id + ". Intentalo de nuevo");
        })
    }


    /**
     * Devuelve la etiqueta de texto que se muestra a la hora de publicar/despublicar un vídeo
     * @param {Integer} id 
     */
    getEtiquetaTextoConfirmacionPublicacion(id) {
        var mensaje = "";
        var key = id + "_publico";

        if(id!==undefined && document.getElementById(key)!==undefined) {
            mensaje = (document.getElementById(key).value==="true")?"¿Deseas despublicar el vídeo con id #" + id + "?":"¿Deseas publicar el vídeo con id #" + id + "?";
        } 
        return mensaje;
    }

    /**
     * Función para manejar el evento de que permite ocultar o mostrar un vídeo
     * @param {Integer} id: Id del vídeo 
     */
    handleOcultarVideo(id) {
        this.limpiarMensajeError();

        if(id===null || id===undefined) {
            this.setEstado([],'',true,"No se puede ocultar el vídeo puesto que su id es desconocido");
        } else {
            let mensaje = this.getEtiquetaTextoConfirmacionPublicacion(id);
            // Se renderiza el componente WindowModalLogin indicando en la propiedad que se debe mostrar
            ReactDOM.render(<ModalConfirmation show={true} title="Atención" onConfirm={()=>this.onConfirmPublicacionVideo(id)} message={mensaje} textButtonConfirm="Aceptar" textButtonCancel="Cancelar"/>, document.getElementById('ventanaModal'));
        }
    }


    /**
     * Función invocada en el caso de que el usuario haya 
     * @param {Integer} id 
     * @param {Integer} vallue 
     */
    onConfirmPublicacionVideo(id) {
        var key = id + "_publico";
    
        if(document.getElementById(key)!==null && document.getElementById(key)!==undefined 
            && document.getElementById(key).value!==null && document.getElementById(key).value!==undefined) {
            var value = document.getElementById(key).value;
                

            VideotecasFacade.publicarVideo(id,this.getValorPublicar(value))
            .then(resultado=>{   
            
                if(resultado.codStatus===200) {
                    // Actualizar imagen
                    let keyImagen = id + "_img";
                    document.getElementById(key).value = this.getValorPublicar(value);
                    document.getElementById(keyImagen).src   = this.getImagenPublicar(this.getValorPublicar(value));
                } else {
                    this.mostrarMensajeError("Se ha producido un error al publicar/despublicar el vídeo");
                }
                
            }).catch(err=>{
                this.mostrarMensajeError("Se ha producido un error al publicar/despublicar el vídeo");
            });
        
        }
    }


    /**
     * Devuelve el valor distinto al valor del parámetro público.
     * @param {String} publico Si valor='0' => se devuelve '1' y si tiene valor=1 => Se devuelve 0
     * @return Integer
     */
    getValorPublicar(publico) {
        let salida = 1;
        if(publico!==undefined && publico!==null && publico==="true") {
            salida = 0;
        }
        return salida;
    }

   /**
     * Devuelve la imagen correspondiente al valor del parámetro publico
     * @param {String} publico Si valor='0' => se devuelve '/images/ojo_cerrado.png' y si tiene valor=1 => Se devuelve '/images/ojo_abierto.png'
     * @return Integer
     */
    getImagenPublicar(publico) {
        let salida = "/images/ojo_cerrado.png";
        if(publico!==undefined && publico!==null && publico===1) {
            salida = "/images/ojo_abierto.png";
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
        }
    }


    /**
     * Limpia el área de mensajes de error de cualquier contenido
     */
    limpiarMensajeError() {
        this.areaMensajeError.current.innerHTML = "";
    }


    /**
     * Método que renderiza la vista
     */
    render() { 

        if(this.hayErrores()===true)  {
            // Sino se ha comunicado una idVideoteca => Entonces se muestra la pantalla de error
            return (
                <ErrorMessage mensaje={this.getMensajeError()}/>
            );
        }else {
            return (
               
                <div className="container">
                    <div className="subtitulo">
                        <h2>{this.state.nombreVideoteca}</h2>
                        <hr></hr>
                    </div>

                    <Breadcrumb>
                        <Breadcrumb.Item href="/">Inicio</Breadcrumb.Item>
                        <Breadcrumb.Item href="/pr_videotecas">
                            Videotecas
                        </Breadcrumb.Item>                                                
                        <Breadcrumb.Item href={'/pr_videoteca/edit/' + this.props.match.params.p_videoteca_id}> 
                            Edición
                        </Breadcrumb.Item>

                        <Breadcrumb.Item href={'/pr_video_adjuntar/' + this.props.match.params.p_videoteca_id}> 
                            Adjuntar vídeo                 
                        </Breadcrumb.Item>              

                        <Breadcrumb.Item active>Detalle</Breadcrumb.Item>
                    </Breadcrumb>

                    <div className="row">
                        <div ref={this.areaMensajeError} className="mensajeError"/>
                        
                    </div>
                
                    <div className="row">
                        {this.state.videos.map((value, index) => {                            
                            let key = value.id + "_publico";
                            let keyImage = value.id + "_img";
                            let imagen = value.publico===true?'/images/ojo_abierto.png':'/images/ojo_cerrado.png';

                            return (

                                <div key={value.id} className="contenedorVideo">

                                    <VisorVideo video={value}/>

                                    <div className="botoneraVideo">                                        
                                        <img src={imagen} id={keyImage} name={keyImage} border="0" width="26" height="26" title="" alt="" onClick={()=>this.handleOcultarVideo(value.id)}/>
                                        <img src="/images/full_trash.png" border="0" width="20" height="20" title="Eliminar" alt="Eliminar" onClick={()=>this.handleEliminar(value.id)}/>
                                    </div>


                                    <input type="hidden" id={key} name={key} value={value.publico}/>
                                    <p className="idVideoFoto">ID #{value.id}</p>
                                    <p className="idVideoFoto">Alta el {value.fechaAltaFormato}</p>
                                    <p className="nombreVideoFoto">{value.nombre}</p>                                
                                
                                
                                </div>
                            );
                        })}
                    </div>
                </div>
              );
        }
    }
}

export default DetalleVideoteca;