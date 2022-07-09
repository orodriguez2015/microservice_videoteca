import React from 'react';
import {AlbumFacade} from '../../../facade/AlbumFacade';
import {PhotoFacade} from '../../../facade/PhotoFacade';
import {StringUtil} from '../../../util/StringUtil';
import ErrorMessage from '../../error/ErrorMessage';
import {URL_BACKEND_IMAGES} from '../../../constantes/Configuracion';
import {ELEMENT} from '../../../constantes/Constantes';
import { SRLWrapper} from 'simple-react-lightbox'; 

/**
 * Componente DetalleAlbumPublico que muestra un listado de los álbumes cuyos administradores
 * han decidido que sean públicos
 * 
 * @author <a href="mailto:oscar.rodriguezbrea@gmail.com">Óscar Rodríguez</a>
 */
class DetalleAlbumPublico extends React.Component {

    /**
     * Constructor
     * @param {Properties} props 
     */
    constructor(props) {
        super(props);
        this.state = {
            fotos :[],
            nombreAlbum:'',
            descErrorAlbum:'',
            error: false
        }
    }

    /**
     * Método componentDidMount, desde que que se recuperan los álbumes públicos
     */
    componentDidMount() {
        if(this.props.location.state===null || this.props.location.state===undefined) {
            // Se comprueba si está definido el state en la llamada a este componente que llega a través de un "a href", como 
            // no lo está habrá que mostrar un error
            this.setState({
                fotos:[],
                nombreAlbum:'',
                descErrorAlbum:'Álbum desconocido',
                error: true
            });
        } else {
            // Se recupera el id del álbum pasado como parámetro
            var idAlbum = this.props.match.params.p_album_id;
            var nombreAlbum = this.props.location.state.nombre;
            
            if(StringUtil.isNotEmpty(idAlbum)) { 
                AlbumFacade.getFotosAlbumPublico(idAlbum)
                .then(resultado=>{

                    if(resultado.codStatus===200) {
                        this.setState({
                            fotos: resultado.data.fotos,
                            nombreAlbum: nombreAlbum
                        });
                    } else {
                        this.showErrorMessage('No se ha podido recuperar las fotografías del álbum');
                    }


                }).catch(err=>{
                    this.showErrorMessage('No se ha podido recuperar las fotografías del álbum');
                });
                
            } else {
                //console.log("no se ha recuperado el idAlbum = " + idAlbum);
                this.showErrorMessage('No se ha podido recuperar las fotografías del álbum');
            }
        }
    }


    /**
     * Muestra un mensaje de error
     * @param {String} message 
     */
    showErrorMessage(message) {
        this.setState({
            fotos:[],
            nombreAlbum:'',
            descError: message,
            error: true
        });
    }


    /**
     * Función invocada cuando se cambia de foto a través del lightbox
     * @param {Object} object 
     */
    onHandleDisplayPhoto(object) {

        if(object!==undefined) {
            let selectedPhotoId = this.state.fotos[object.index].id;
            
            this.increasePhotoDisplayCounter(selectedPhotoId);
        }
    }


    /**
     * Función invocada cuando se abre el lightbox seleccionando una fotografía
     * @param {Object} object 
     */
    onLightboxOpened(object) {
        if(object!==undefined) {
            
            // Se obtiene la posición de la foto en la lista de fotos
            var position = String(object.currentSlide.id);
            position = position.replace(ELEMENT,'');
            // Se recupera la foto en base a la posición para recuperar el id de la foto
            //let selectedPhotoId = this.state.fotos[position].id;
            this.increasePhotoDisplayCounter(this.state.fotos[position].id);
           
        }
    }


    /**
     * Incrementa el contador de visualizaciones de una fotografía invocando al servidor, y actualiza el número de visualizaciones
     * en pantalla
     * @param {Integer} Id de la fotografía
     */
    increasePhotoDisplayCounter(id) {
        PhotoFacade.increasePhotoDisplayCounter(id).then(resultado=>{

            if(resultado.codStatus===200) {
                // Se ha incrementa el contador de visualizaciones de la foto => Se actualiza el contador en pantalla
                document.getElementById(id).innerHTML = resultado.data.numeroVisualizaciones;
            }

        }).catch(err=>{
            console.log("error = " + err.message);
        })
    }

   

    /**
     * Método que renderiza la vista
     */
    render() { 
        if(this.state.error===true) {
            // Sino se ha comunicado un idAlbum => Entonces se muestra la pantalla de error
            return (
                <ErrorMessage mensaje={this.state.descError}/>
            );
        } else {

            const options = {
                overlayColor: "rgb(0, 0, 0.9)",
                showCaption: false,
                buttonsBackgroundColor: "rgba(0, 0, 0, 0.9)",
                buttonsIconColor: "rgba(219, 219, 219, 0.7)",
                showThumbnails: true,
                transitionSpeed: 200,
                transitionTimingFunction: "linear"
            };


            const callbacks = {
                onSlideChange: object => this.onHandleDisplayPhoto(object),
                onLightboxOpened: object => this.onLightboxOpened(object)
               
            };

            return (
                <SRLWrapper options={options} callbacks={callbacks}>
                <div className="container">
                    <div className="subtitulo">
                        <h2>Fotografías del álbum {this.state.nombreAlbum}</h2>
                        <hr></hr>
                    </div>
                
                    <div className="row">
                        {this.state.fotos.map((value, index) => {
                        
                            let imgOriginal  = URL_BACKEND_IMAGES + value.rutaRelativa;
                            
                            return <div key={value.id} className="col-3">
                
                                        <a href={`${imgOriginal}`} data-attribute="SRL">
                                            <img src={`${imgOriginal}`} alt={`${imgOriginal}`} width="200" height="150"/>
                                        </a>
                                        <p className="nombreVideoFoto">{value.nombre}</p>                        
                                        <p className="idVideoFoto">{value.descripcion}</p>
                                        <p className="idVideoFoto">ID # {value.id}</p>
                                        <p className="idVideoFoto">Visto <span id={value.id} name={value.id}>{ value.numeroVisualizaciones }</span>  veces</p>
                                        <p className="idVideoFoto">Alta el {value.fechaAltaFormato}</p>
                                        
                                        
                                </div>
                        })}
                    </div>
                </div>
                </SRLWrapper>
              );
        }
    }
}
export default DetalleAlbumPublico;