import React from 'react';
import { Player } from 'video-react';
import {URL_BACKEND} from '../../../constantes/Configuracion';

/**
 * Componente que permite visualizar un vídeo a través del componente de video-react
 * @author <a href="mailto:oscar.rodriguezbrea@gmail.com">Óscar Rodríguez Brea</a>
 */
class VisorVideo extends React.Component {

    /**
     * Método que renderiza la vista
     */
    render() {
        let rutaVideo = URL_BACKEND + this.props.video.ruta_relativa;
        const autoPlay = false;
        const playsInline = true;
        const fluid = true;
        const preload = "metadata";

        return (
            <div>
                <Player
                    autoPlay = {autoPlay}
                    playsInline = {playsInline}
                    fluid = {fluid}
                    preload = {preload}
                    src={rutaVideo}/>
            </div>
        );
    }
}
export default VisorVideo;