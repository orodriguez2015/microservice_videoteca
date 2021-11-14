import React from 'react';
import {AlbumFacade} from '../../../facade/AlbumFacade';
import {StringUtil} from '../../../util/StringUtil';
import ErrorMessage from '../../error/ErrorMessage';
import {URL_BACKEND} from '../../../constantes/Configuracion';
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
            errorAlbumDesconocido: false
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
                errorAlbumDesconocido:true
            });
        } else {
            // Se recupera el id del álbum pasado como parámetro
            var idAlbum = this.props.match.params.p_album_id;
            var nombreAlbum = this.props.location.state.nombre;

            if(StringUtil.isNotEmpty(idAlbum)) { 

                AlbumFacade.getFotosAlbumPublico(idAlbum)
                .then(resultado=>{

                    this.setState({
                        fotos: resultado.fotos,
                        nombreAlbum: nombreAlbum
                    });

                }).catch(err=>{
                    console.log("error= " + err.message);
                });
                
            } else {
                console.log("no se ha recuperado el idAlbum = " + idAlbum);
            }
        }
    }

    /**
     * Método que renderiza la vista
     */
    render() { 
        if(this.state.errorAlbumDesconocido===true) {
            // Sino se ha comunicado un idAlbum => Entonces se muestra la pantalla de error
            return (
                <ErrorMessage mensaje={"Álbum desconocido"}/>
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

            return (
                <SRLWrapper options={options}>
                <div className="container">
                    <div className="subtitulo">
                        <h2>Fotografías del álbum {this.state.nombreAlbum}</h2>
                        <hr></hr>
                    </div>
                
                    <div className="row">
                        {this.state.fotos.map((value, index) => {
                            // Se construye la ruta de la miniatura en el servidor
                            let imgMiniatura = URL_BACKEND + value.RUTAMINIATURA;
                            let imgOriginal  = URL_BACKEND + value.RUTA;
                            
                            return <div key={value.ID} className="col-3">
                
                                        <a href={`${imgOriginal}`} data-attribute="SRL">
                                            <img src={`${imgMiniatura}`} alt={`${imgMiniatura}`}/>
                                        </a>
                                        <p className="nombreVideoFoto">{value.NOMBRE}</p>                        
                                        <p className="idVideoFoto">{value.DESCRIPCION}</p>
                                        <p className="idVideoFoto">Alta el {value.FECHA_ALTA}</p>
                                        <p className="idVideoFoto">Visto { value.NUMEROVISUALIZACIONES }  veces</p>
                                        <p className="idVideoFoto">ID # {value.ID}</p>
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