import React from 'react';
import ReactDOM from 'react-dom';
import {AlbumFacade} from '../../../facade/AlbumFacade';
import {PhotoFacade} from '../../../facade/PhotoFacade';
import LightBoxImages from '../common/LightBoxImages.jsx';
import {StringUtil} from '../../../util/StringUtil';
import ErrorMessage from '../../error/ErrorMessage';
import {URL_BACKEND_IMAGES} from '../../../constantes/Configuracion';


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
     * Muestra la caja de luz que permite navegar y visualizar las imágene
     * @param {Integer} index Indice de la foto en el array de fotografías
     * @param {Array} imageList contiene el array con la localización de las imágenes 
     */
    showLightBox(index,imageList) {
        ReactDOM.render(<LightBoxImages photoIndex={index} images={imageList} onCallbackActivateCounterDisplay={this.onCallbackActivateCounterDisplay} activatePhotoDisplayCount={true}/>, document.getElementById('ventanaModal'));
    }
   

    /**
     * Callback que se invoca cuando se visualiza una fotografía
     * @param {int} id 
     */
    onCallbackActivateCounterDisplay(id) {
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

            let images =  [];
            
            this.state.fotos.map((value, index) => {
                images.push({
                    id:value.id,
                    url: URL_BACKEND_IMAGES + value.rutaRelativa
                });
            });

            return (
               
                <div className="container">
                    <div className="subtitulo">
                        <h2>Fotografías del álbum {this.state.nombreAlbum}</h2>
                        <hr></hr>
                    </div>
                
                    <div className="row">
                        {this.state.fotos.map((value, index) => {
                        
                            let imgOriginal  = URL_BACKEND_IMAGES + value.rutaRelativaMiniatura;
                            
                            return (
                            
                                 <div key={value.id} className="contenedorAlbum">

                                    <div className="contenedorFoto">
                                        <img src={`${imgOriginal}`} id={`${imgOriginal}`} alt={`${imgOriginal}`} onClick={()=>this.showLightBox(index,images)}/>
                                    </div>

                                    <div className='pieFoto'>
                                        <p className="nombreFoto">{value.nombre}</p>                        
                                        <p className="idFoto">{value.descripcion}</p>
                                        <p className="idFoto">ID # {value.id}</p>
                                        <p className="idFoto">Visto <span id={value.id} name={value.id}>{ value.numeroVisualizaciones }</span>  veces</p>
                                        <p className="idFoto">Alta el {value.fechaAltaFormato}</p>
                                    </div>
                                        
                                </div>
                            )
                        })}
                    </div>
                </div>
                
              );
        }
    }
}
export default DetalleAlbumPublico;