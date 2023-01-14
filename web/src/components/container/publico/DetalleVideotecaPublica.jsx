import React from 'react';
import {VideotecasFacade} from '../../../facade/VideotecasFacade';
import {StringUtil} from '../../../util/StringUtil';
import ErrorMessage from '../../error/ErrorMessage';
import VisorVideo from '../video/VisorVideo';



/**
 * Componente que permite visualizar los vídeos de una determinada videoteca
 * @author <a href="mailto:oscar.rodriguezbrea@gmail.com">Óscar Rodríguez Brea</a>
 */
class DetalleVideotecaPublica extends React.Component {

    /**
     * Constructor
     * @param {Properties} props 
     */
    constructor(props) {
        super(props);
        this.state = {
            videos:[],
            nombreVideoteca:'',
            errorVideoteca: false,
            descripcionError:''
        }
    }

    /**
     * Método componentDidMount, desde que que se recuperan los álbumes públicos
     */
    componentDidMount() { 
        if(this.props.location.state===null || this.props.location.state===undefined) {
            // Se comprueba si está definido el state en la llamada a este componente que llega a través de un "a href", como 
            // no lo está habrá que mostrar un error
            this.setEstado([],'',true,'Videoteca desconocida');

        } else {
            // Se recupera el id del videoteca pasado como parámetro en la url
            var idVideoteca = this.props.match.params.p_videoteca_id;
            // Se recupera el nombre de la videoteca pasada en el estado en la petición de carga del componente
            var nombreVideoteca = this.props.location.state.nombre;

            if(StringUtil.isNotEmpty(idVideoteca)) { 

                VideotecasFacade.getVideosFromVideoteca(idVideoteca)
                .then(resultado=>{
                    this.setEstado(resultado.data.videos,resultado.data.nombre,false,'');
            
                }).catch(err=>{
                    this.setEstado([],nombreVideoteca,true,"Se ha producido un error al recuperar los vídeos que forman la videoteca");
                });
                
            } else {
                this.setEstado([],nombreVideoteca,true,"Videoteca desconocida");
            }
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
            errorVideoteca: error,
            descripcionError: StringUtil.isNotEmpty(descripcionError)?descripcionError:'Videoteca desconocida'
        });
    }


    /**
     * Método que renderiza la vista
     */
    render() { 
        if(this.state.errorVideoteca===true) {
            // Sino se ha comunicado una idVideoteca => Entonces se muestra la pantalla de error
            return (
                <ErrorMessage mensaje={this.state.descripcionError}/>
            );
        } else {

            return (
                
                <div className="container">
                    <div className="subtitulo">
                        <h2>{this.state.nombreVideoteca}</h2>
                        <hr></hr>
                    </div>
                
                    <div className="row">
                        {this.state.videos.map((value,index,array) => {                            

                        return (
                            <div key={value.id} className="contenedorVideo">
                                <VisorVideo video={value}/>
                                <p className="nombreVideoFoto">{value.nombre}</p>
                                <p className="idVideoFoto">Alta el {value.fechaAltaFormato}</p>
                                <p className="idVideoFoto">ID #{value.id}</p>

                            </div>
                        );
                        })}
                    </div>
                </div>
              );
        }
    }
}

export default DetalleVideotecaPublica;