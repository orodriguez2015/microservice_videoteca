import React from 'react';
import {AlbumFacade} from '../../../facade/AlbumFacade';
import { NavLink } from 'react-router-dom';

/**
 * Componente Albumes que muestra un listado de los álbumes cuyos administradores
 * han decidido que sean públicos
 * @author <a href="mailto:oscar.rodriguezbrea@gmail.com">Óscar Rodríguez</a>
 */
class AlbumesPublicos extends React.Component {

    /**
     * Constructor
     * @param {Properties} props 
     */
    constructor(props) {
        super(props);
        this.state = {
            albumes :[]
        }
    }

    /**
     * Método componentDidMount, desde que que se recuperan los álbumes públicos
     */
    componentDidMount() {
        AlbumFacade.getAlbumesPublicos()
        .then(resultado=>{
            this.setState({
                albumes: resultado
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
                    <h2>Álbumes fotográficos de los usuarios</h2>
                    <hr></hr>
                </div>
            
                <p>Desde esta sección tendrás acceso a las fotografías que los usuarios han publicado</p>
     
                {this.state.albumes.map((value, index) => {
                    return <div key={value.id}>
                                <NavLink to={{ pathname:`p_album/${value.id}`,state:{nombre:`${value.nombre}`}}} className="nombreAlbum">{value.nombre}</NavLink>
                                <p>{value.descricion}</p>
                                <p>Dado de alta el {value.fechaAlta} por {value.nombreCompleto}</p>
                           </div>
                })}
            </div>
        );
    }
}

export default AlbumesPublicos;