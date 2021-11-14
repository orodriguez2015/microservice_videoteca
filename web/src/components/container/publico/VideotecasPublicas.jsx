import React from 'react';
import {VideotecasFacade} from '../../../facade/VideotecasFacade';
import { NavLink } from 'react-router-dom';

/**
 * Componente VideotecasPublicas. Muestra un listado de videotecas
 * públicas accesibles, a las que los usuarios podrán acceder
 * 
 * @author <a href="mailto:oscar.rodriguezbrea@gmail.com">Óscar Rodríguez</a>
 */
class VideotecasPublicas extends React.Component {

    /**
     * Constructor
     * @param {Properties} props 
     */
    constructor(props) {
        super(props);
        this.state = {
            videotecas :[]
        }
    }

    /**
     * Método componentDidMount, desde que que se recuperan los álbumes públicos
     */
    componentDidMount() {
        VideotecasFacade.getVideotecasPublicas()
        .then(resultado=>{
            this.setState({
                videotecas: resultado
            });
    
        }).catch(err=>{
            console.log("error= " + err.message);
        });
    }

    /**
     * Método que renderiza la vista
     */
    render() { 
        return (
            <div className="container">
                <div className="subtitulo">
                    <h2>Videotecas de los usuarios</h2>
                    <hr></hr>
                </div>
            
                <p>Desde esta sección tendrás acceso a las videotecas públicas de los usuarios.</p>
     
                {this.state.videotecas.map((value, index) => {
                    return <div key={value.id}>
                                <NavLink to={{ pathname:`p_videos/${value.id}`,state:{nombre:`${value.nombre}`}}} className="nombreAlbum">{value.nombre}</NavLink>
                                <p>Dado de alta el {value.fechaAlta} por {value.nombreUsuario}</p>
                           </div>
                })}
            </div>
        );
    }
}

export default VideotecasPublicas;