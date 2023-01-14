import {AlmacenFacade} from '../store/AlmacenFacade';

import { VIDEOTECAS_PUBLICO_API ,VIDEOTECAS_API, VIDEO_API,
    PUBLICAR_VIDEO_API,SUBMIT_VIDEO_API,GET_VIDEOS_API,PUBLIC_VIDEOS_API} from '../constantes/Configuracion';

/**
 * 
 * Clase con operaciones relacigetVideosFromVideotecaonadas con peticiones al backend para realizar 
 * operaciones con las videotecas
 * 
 * @author <a href="mailto:oscar.rodriguezbrea@gmail.com">Óscar Rodríguez</a>
 */
export class VideotecasFacade {

    /**
     * Se envía una petición al servidor para recuperar la información básica de los 
     * videotecas públicas de las que se puede visualizar sus vídeos
     * @return Una promesa
     */
    static getVideotecasPublicas() {
        let headers =  {
            "Content-Type": "application/json",
            "Access-Control-Request-Headers": "*",
            "Access-Control-Request-Method": "*",
            'Access-Control-Allow-Origin':"*",
            'Access-Control-Allow-Credentials': 'true'
        }

    
        var opciones = {
            method: 'GET',
            mode: 'cors',
            headers: headers
        }

        return new Promise((resolver, rechazar) => {
            fetch(VIDEOTECAS_PUBLICO_API,opciones)
            .then((response) => {
                return response.json();
            })
            .then((recurso) => { 
                resolver(recurso);
            }).catch(err=>{ 
                rechazar(err);
            });
        });
    }




    /**
     * Se envía una petición al servidor para recuperar los vídeos que conforman una 
     * determinada videoteca , y que a su vez son de un determinado usuario
     * @param idVideoteca Id de la videoteca
     * @param idUsuario Id del usuario
     * @return Una promesa
     */
    static getVideosFromVideoteca(idVideoteca,idUsuario) {
        let headers =  {
            "Content-Type": "application/json",
            "Access-Control-Request-Headers": "*",
            "Access-Control-Request-Method": "*"
        }

        var opciones = {
            method: 'GET',
            mode: 'cors',
            headers: headers
        }

        return new Promise((resolver, rechazar) => {
            //fetch(GET_VIDEOS_API + idVideoteca,opciones)
            fetch(PUBLIC_VIDEOS_API + idVideoteca,opciones)
            .then((response) => {
                return response.json();
            })
            .then((recurso) => { 
                resolver(recurso);
            }).catch(err=>{ 
                rechazar(err);
            });
        });
    }


    /**
     * Se envía una petición al servidor para grabar la videoteca de un usuario
     * determinada videoteca 
     * @param dVideoteca Id de la videoteca
     * @return Una promesa
     */
    static save(nombre,publico) {
        let headers =  {
            "Content-Type": "application/json",
            "Access-Control-Request-Headers": "*",
            "Access-Control-Request-Method": "*",
            "Authorization" : AlmacenFacade.getUser().authenticationToken
        }

        var opciones = {
            method: 'POST',
            mode: 'cors',
            headers: headers,
            body: JSON.stringify({
                nombre: nombre,
                publico: publico,
                idUsuario: AlmacenFacade.getUser().id
            })
        }

        return new Promise((resolver, rechazar) => {
            fetch(VIDEOTECAS_API,opciones)
            .then((response) => {
                return response.json();
            })
            .then((recurso) => { 
                resolver(recurso);
            }).catch(err=>{ 
                rechazar(err);
            });
        });
    }



   /**
     * Se envía una petición al servidor para editar una videoteca de un usuario
     * determinada videoteca 
     * @param dVideoteca Id de la videoteca
     * @return Una promesa
     */
    static updateVideoteca(nombre,publico,idVideoteca) {
        let headers =  {
            "Content-Type": "application/json",
            "Access-Control-Request-Headers": "*",
            "Access-Control-Request-Method": "*",
            "Authorization" : AlmacenFacade.getUser().authenticationToken
        }

        var opciones = {
            method: 'PUT',
            mode: 'cors',
            headers: headers,
            body: JSON.stringify({
                nombre: nombre,
                publico: publico,
                idUsuario: AlmacenFacade.getUser().id,
                id: idVideoteca
            })
        }

        return new Promise((resolver, rechazar) => {
            fetch(VIDEOTECAS_API + "/" + idVideoteca,opciones)
            .then((response) => {
                return response.json();
            })
            .then((recurso) => { 
                resolver(recurso);
            }).catch(err=>{ 
                rechazar(err);
            });
        });
    }




   /**
     * Se envía una petición al servidor para recuperar los videotecas de un usuario 
     * @param idUsuario Id del usuario
     * @return Una promesa
     */
    static getVideosUsuario(idUsuario) {
        let headers =  {
            "Content-Type": "application/json",
            "Access-Control-Request-Headers": "*",
            "Access-Control-Request-Method": "*",
            "Authorization" : AlmacenFacade.getUser().authenticationToken
        }

        var opciones = {
            method: 'GET',
            mode: 'cors',
            headers: headers
        }

        return new Promise((resolver, rechazar) => {
            fetch(VIDEOTECAS_API + "/" + AlmacenFacade.getUser().id,opciones)
            .then((response) => {
                return response.json();
            })
            .then((recurso) => { 
                resolver(recurso);
            }).catch(err=>{ 
                rechazar(err);
            });
        });
    }


    /**
     * Se envía una petición al servidor para eliminar una determinada videoteca
     * en el sistema, junto con todas sus vídeos
     * @param {Integer} idVideoteca: Id de la videoteca
     * @param {Integer} idUsuario : Id del usuario
     * @return Una promesa
     */
    static deleteVideoteca(idVideoteca,idUsuario) {
        let headers =  {
            "Content-Type": "application/json",
            "Access-Control-Request-Headers": "*",
            "Access-Control-Request-Method": "*",
            "Authorization" : AlmacenFacade.getUser().authenticationToken
        }

        var opciones = {
            method: 'DELETE',
            mode: 'cors',
            headers: headers
        };

        return new Promise((resolver, rechazar) => {
            fetch(VIDEOTECAS_API +  "/" + idVideoteca + "/" + idUsuario,opciones)
            .then((response) => {
                return response.json()
            })
            .then((recurso) => { 
                resolver(recurso);
            }).catch(err=>{ 
                rechazar(err);
            });
        }); 
    }


   /**
     * Se envía una petición al servidor para recuperar una determinada videoteca
     * @param {Integer} idVideoteca: Id de la videoteca
     * @param {Integer} idUsuario : Id del usuario propietario de la videoteca
     * @return Una promesa
     */
    static getVideoteca(idVideoteca) {
        let headers =  {
            "Content-Type": "application/json",
            "Access-Control-Request-Headers": "*",
            "Access-Control-Request-Method": "*",
            "Authorization" : AlmacenFacade.getUser().authenticationToken
        }

        var opciones = {
            method: 'GET',
            mode: 'cors',
            headers: headers
        };


        return new Promise((resolver, rechazar) => {
            fetch(VIDEOTECAS_API + "/" + idVideoteca + "/" + AlmacenFacade.getUser().id,opciones)
            .then((response) => {
                return response.json()
            })
            .then((recurso) => { 
                resolver(recurso);
            }).catch(err=>{ 
                rechazar(err);
            });
        }); 
    }

    /**
     * Se envía una petición al servidor para eliminar un determinado video
     * @param {Integer} idVideo Id del video
     * @return Una promesa
     */
    static deleteVideo(idVideo) {
        let headers =  {
            "Content-Type": "application/json",
            "Access-Control-Request-Headers": "*",
            "Access-Control-Request-Method": "*",
            "Authorization" : AlmacenFacade.getUser().authenticationToken
        }

        var opciones = {
            method: 'DELETE',
            mode: 'cors',
            headers: headers
        };

        return new Promise((resolver, rechazar) => {
            fetch(VIDEO_API +  "/" + idVideo,opciones)
            .then((response) => {
                return response.json()
            })
            .then((recurso) => { 
                resolver(recurso);
            }).catch(err=>{ 
                rechazar(err);
            });
        }); 
    }


   

    /**
     * Se envía una petición al servidor para publicar o despublicar un vídeo. 
     * La petición la tiene que hacer el usuario propietario del vídeo
     * @param {Integer} idVideo: Id del video
     * @param {Integer} value  : Id del usuario
     * @return Una promesa
     */
    static publicarVideo(idVideo,value) {
        let headers =  {
            "Content-Type": "application/json",
            "Access-Control-Request-Headers": "*",
            "Access-Control-Request-Method": "*",
            "Authorization" : AlmacenFacade.getUser().authenticationToken
        }

        var opciones = {
            method: 'PUT',
            mode: 'cors',
            headers: headers
        };

        return new Promise((resolver, rechazar) => {
            fetch(PUBLICAR_VIDEO_API + idVideo + "/" + AlmacenFacade.getUser().id + "/" + value,opciones)
            .then((response) => {
                return response.json()
            })
            .then((recurso) => { 
                resolver(recurso);
            }).catch(err=>{ 
                rechazar(err);
            });
        }); 
    }



    /**
     * Se envía una petición al servidor para publicar o despublicar un vídeo. 
     * La petición la tiene que hacer el usuario propietario del vídeo
     * @param {Integer} idVideoteca: Id de la videoteca
     * @param {File} ficheros: Fichero a enviar al servidor
     * @return Una promesa
     */
    static submitVideo(idVideoteca,ficheros) {
        let headers =  {
            "Access-Control-Request-Headers": "*",
            "Access-Control-Request-Method": "*",
            "Authorization" : AlmacenFacade.getUser().authenticationToken
        }

        var formData = new FormData();
        formData.append("idVideoteca",idVideoteca);
        formData.append("idUsuario",AlmacenFacade.getUser().id);

        if(ficheros!==undefined) {
            Object.values(ficheros).forEach(element => {
                formData.append("ficheros",element);
            });
        }

        var opciones = {
            method: 'POST',
            mode: 'cors',
            headers: headers,
            body: formData
        };

        return new Promise((resolver, rechazar) => {
            fetch(SUBMIT_VIDEO_API + idVideoteca + "/" + AlmacenFacade.getUser().id,opciones)
            .then((response) => {
                return response.json()
            })
            .then((recurso) => { 
                resolver(recurso);
            }).catch(err=>{ 
                rechazar(err);
            });
        }); 
    }

}