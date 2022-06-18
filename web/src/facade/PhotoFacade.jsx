
/**
 * Clase PhotoFacade
 */
export class PhotoFacade {

    /**
     * Incrementa el contador de visualizaciones de una determinada fotografía
     * @param {Integer} id 
     */
    static increasePhotoDisplayCounter(id) {

        let headers =  {
            "Content-Type": "application/json",
            "Access-Control-Request-Headers": "*",
            "Access-Control-Request-Method": "*",
        }

        var opciones = {
            method: 'POST',
            mode: 'cors',
            headers: headers
        }

        return new Promise((resolver, rechazar) => {
            fetch(ALBUM_DETAIL_API + idAlbum + "/" + user.id,opciones)
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