import {AlmacenFacade} from '../store/AlmacenFacade';

import { VIDEOTECAS_PUBLICO_API, VIDEOS_VIDEOTECA_PUBLICO_API ,VIDEOTECAS_API, VIDEO_API,
    PR_VIDEOS_API,PUBLICAR_VIDEO_API,SUBMIT_VIDEO_API} from '../constantes/Configuracion';





/**
 * 
 * Clase con operaciones relacionadas con peticiones al backend para realizar 
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
     * determinada videoteca 
     * @param idVideoteca Id de la videoteca
     * @return Una promesa
     */
    static getVideos(idVideoteca) {
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
            fetch(VIDEOS_VIDEOTECA_PUBLICO_API + "/" + idVideoteca,opciones)
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
            fetch(PR_VIDEOS_API + idVideoteca + "/" + idUsuario,opciones)
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
            "Access-Control-Request-Method": "*"
        }

        var opciones = {
            method: 'DELETE',
            mode: 'cors',
            headers: headers,
            body: JSON.stringify({
                idUsuario: idUsuario,
                idVideoteca: idVideoteca
            })
        };

        return new Promise((resolver, rechazar) => {
            fetch(VIDEOTECAS_API +  "/" + idVideoteca,opciones)
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
     * @param {Integer} idVideo: Id del video
     * @param {Integer} idUsuario : Id del usuario
     * @return Una promesa
     */
    static deleteVideo(idVideo,idUsuario) {
        let headers =  {
            "Content-Type": "application/json",
            "Access-Control-Request-Headers": "*",
            "Access-Control-Request-Method": "*"
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
     * @param {Integer} idUsuario: Id del usuario que tiene que ser el propietario del vídeo
     * @param {Integer} value  : Id del usuario
     * @return Una promesa
     */
    static publicarVideo(idVideo,idUsuario,value) {
        let headers =  {
            "Content-Type": "application/json",
            "Access-Control-Request-Headers": "*",
            "Access-Control-Request-Method": "*"
        }

        var opciones = {
            method: 'POST',
            mode: 'cors',
            headers: headers,
            body: JSON.stringify({
                idVideo: idVideo,
                publico: value,
                idUsuario: idUsuario
            })
        };

        return new Promise((resolver, rechazar) => {
            fetch(PUBLICAR_VIDEO_API,opciones)
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
     * @param {Integer} idUsuario: Id del usuario que ejecuta la operación
     * @param {File} fichero: Fichero a enviar al servidor
     * @return Una promesa
     */
    static submitVideo(idVideoteca,idUsuario,fichero) {
        let headers =  {
            "Access-Control-Request-Headers": "*",
            "Access-Control-Request-Method": "*"
        }

        var formData = new FormData();
        formData.append("idUsuario",idUsuario);
        formData.append("fichero",fichero);

        var opciones = {
            method: 'POST',
            mode: 'cors',
            headers: headers,
            body: formData
        };

        return new Promise((resolver, rechazar) => {
            fetch(SUBMIT_VIDEO_API + "/" + idVideoteca,opciones)
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