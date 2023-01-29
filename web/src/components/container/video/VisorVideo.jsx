import React from 'react';
import { Player } from 'video-react';
import {URL_BACKEND_VIDEOS} from '../../../constantes/Configuracion';

/**
 * Componente que permite visualizar un vídeo a través del componente de video-react
 * @author <a href="mailto:oscar.rodriguezbrea@gmail.com">Óscar Rodríguez Brea</a>
 */
class VisorVideo extends React.Component {

    /**
     * Método que renderiza la vista
     */
    render() {
        let rutaVideo = URL_BACKEND_VIDEOS + this.props.video.rutaRelativa;
        const autoPlay = false;
        const playsInline = true;
        const fluid = false;
        const preload = "metadata";
        const width = 200;
        const height = 190;
        const aspectRatio = "4:3";

        return (
            <div align="center">
                <Player
                    autoPlay = {autoPlay}
                    playsInline = {playsInline}
                    fluid = {fluid}
                    preload = {preload}
                    src={rutaVideo}
                    witdh= {width}
                    height={height}
                    aspectRatio={aspectRatio}/>
            </div>
        );
    }
}
export default VisorVideo;