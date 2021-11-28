import {FOTO_ALBUM_PUBLICO_API,ALBUMES_PUBLICO_API,ALBUM_SAVE_API,ALBUMES_USUARIO_ADMIN_API,ALBUM_ADMIN_API ,PR_FOTO_API,PUBLICAR_FOTO_API,FOTO_ALBUM_ADMIN,URL_ATTACH_PHOTOS} from '../constantes/ConfiguracionAlbumes';

/**
 * ,
 * Clase con operaciones relacionadas con peticiones al backend para realizar 
 * operaciones con álbumes fotográficos
 * 
 * @author <a href="mailto:oscar.rodriguezbrea@gmail.com">Óscar Rodríguez</a>
 */
export class AlbumFacade {

    /**
     * Se envía una petición al servidor para recuperar la información básica de los 
     * álbumes públicos que se visualizan 
     * @return Una promesa
     */
    static getAlbumesPublicos() {
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
            fetch(ALBUMES_PUBLICO_API,opciones)
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
     * Se envía una petición al servidor para comprobar si un usuario existe 
     * en el sistema. 
     * @param idAlbum: Id del álbum
     * @return Una promesa
     */
    static getFotosAlbumPublico(idAlbum) {
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
            fetch(FOTO_ALBUM_PUBLICO_API + "/" + idAlbum,opciones)
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
     * Se envía una petición al servidor para recuperar las fotos de un álbum, esté o no marcadas como publicadas
     * @param idAlbum: Id del álbum
     * @return Una promesa
     */
    static getFotosAlbumPrivado(idAlbum) {
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
            fetch(FOTO_ALBUM_ADMIN + idAlbum,opciones)
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
     * Se envía una petición al servidor para dar de alta un nuevo álbum fotográfico
     * en el sistema. 
     * @param {String} nombreAlbum: Nombre del álbum
     * @param {String} descripcionAlbum : Descripción del álbum
     * @param {Boolean} publico: Indica si el álbum es o no público
     * @param {Integer} idUsuario: Id del usuario
     * @return Una promesa
     */
    static saveAlbum(nombreAlbum,descripcionAlbum,publico,idUsuario) {
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
                nombre: nombreAlbum,
                descripcion: descripcionAlbum,
                publico: publico,
                idUsuario: idUsuario
            })
        };

        return new Promise((resolver, rechazar) => {
            fetch(ALBUM_SAVE_API,opciones)
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
     * Se envía una petición al servidor para dar de alta un nuevo álbum fotográfico
     * en el sistema. 
     * @param {String} nombreAlbum: Nombre del álbum
     * @param {String} descripcionAlbum : Descripción del álbum
     * @param {Boolean} publico: Indica si el álbum es o no público
     * @param {Integer} idUsuario: Id del usuario
     * @return Una promesa
     */
    static updateAlbum(idAlbum,nombreAlbum,descripcionAlbum,publico,idUsuario) {
        let headers =  {
            "Content-Type": "application/json",
            "Access-Control-Request-Headers": "*",
            "Access-Control-Request-Method": "*"
        }

        var opciones = {
            method: 'PUT',
            mode: 'cors',
            headers: headers,
            body: JSON.stringify({
                nombre: nombreAlbum,
                descripcion: descripcionAlbum,
                publico: publico,
                idUsuario: idUsuario,
                idAlbum: idAlbum
            })
        };

    
        return new Promise((resolver, rechazar) => {
            fetch(ALBUM_ADMIN_API + idAlbum,opciones)
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
     * Se envía una petición al servidor para recuperar los álbumes de un usuario
     * @param {Object} user: Objeto con los datos del usuario
     * @return Una promesa
     */
    static getAlbumesUsuario(user) {
        let headers =  {
            "Content-Type": "application/json",
            "Access-Control-Request-Headers": "*",
            "Access-Control-Request-Method": "*",
            "Authorization" : user.authenticationToken
        }

        var opciones = {
            method: 'POST',
            mode: 'cors',
            headers: headers
        };

        return new Promise((resolver, rechazar) => {
            fetch(ALBUMES_USUARIO_ADMIN_API + user.id,opciones)
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
     * Se envía una petición al servidor para recuperar los datos básicos de un álbum
     * @param {Integer} idAlbum: Id del álbum
     * @return Una promesa
     */
    static getAlbum(idAlbum,idUsuario) {
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
                idUsuario: idUsuario
            })
        };

        return new Promise((resolver, rechazar) => {
            fetch(ALBUM_ADMIN_API + idAlbum,opciones)
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
     * Se envía una petición al servidor para eliminar un álbum fotográfico
     * en el sistema, junto con todas sus fotografías 
     * @param {Integer} idAlbum: Id del álbum
     * @param {Integer} idUsuario : Id del usuario
     * @return Una promesa
     */
    static deleteAlbum(idAlbum,idUsuario) {
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
                idAlbum: idAlbum
            })
        };

        return new Promise((resolver, rechazar) => {
            fetch(ALBUM_ADMIN_API + idAlbum,opciones)
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
     * Se envía una petición al servidor para eliminar una fotografía de un álbum y del servidor
     * @param {Integer} idFoto: Id de la foto
     * @param {Integer} idUsuario : Id del usuario
     * @return Una promesa
     */
    static deleteFoto(idFoto,idUsuario) {
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
                idUsuario: idUsuario
            })
        };

        return new Promise((resolver, rechazar) => {
            fetch(PR_FOTO_API + idFoto,opciones)
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
     * Se envía una petición al servidor para publicar o despublicar una fotografía. 
     * La petición la tiene que hacer el usuario propietario del vídeo
     * @param {Integer} idFoto: Id de la foto
     * @param {Integer} idUsuario: Id del usuario que tiene que ser el propietario de la foto
     * @param {Integer} value  : Id del usuario
     * @return Una promesa
     */
    static publicarFoto(idFoto,idUsuario,value) {
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
                publico: value,
                idUsuario: idUsuario
            })
        };

        return new Promise((resolver, rechazar) => {
            fetch(PUBLICAR_FOTO_API + idFoto,opciones)
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
     * Se envía una petición al servidor para adjuntar varias fotografías a un 
     * determinado álbum
     * @param {Integer} idAlbum: Id del álbum
     * @param {Integer} idUsuario: Id del usuario que ejecuta la operación
     * @param {File} ficheros: Ficheros a enviar al servidor
     * @return Una promesa
     */
     static submitFotos(idAlbum,idUsuario,ficheros) {
        let headers =  {
            "Access-Control-Request-Headers": "*",
            "Access-Control-Request-Method": "*"
        }

        var formData = new FormData();
        formData.append("idUsuario",idUsuario);
        
        if(ficheros!==undefined) {
            let indice = 0;
            ficheros.forEach(element => {
                formData.append("fichero" + indice,element);
                indice++;
            });
        }

        var opciones = {
            method: 'POST',
            mode: 'cors',
            headers: headers,
            body: formData
        };

        return new Promise((resolver, rechazar) => {
            fetch(URL_ATTACH_PHOTOS + "/" + idAlbum,opciones)
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