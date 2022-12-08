import mimetypes from '../config/config_images.json';
import mimetypesVideos from '../config/config_videos.json';


/**
 * Clase FileUtil
 * @author <a href="mailto:oscar.rodriguezbrea@gmail.com">Óscar Rodríguez</a>
 */
export default class FileUtil {

    /**
     * Formatea el tamaño en bytes de un fichero y devuelve un String en un formato más adeucado
     * @param {Integer} x 
     * @return String
     */
    static formatSizeUnits(x){
        const units = ['bytes', 'KB', 'MB', 'GB', 'TB', 'PB', 'EB', 'ZB', 'YB'];
        let l = 0, n = parseInt(x, 10) || 0;

        while(n >= 1024 && ++l)
            n = n/1024;

        return(n.toFixed(n >= 10 || l < 1 ? 0 : 1) + ' ' + units[l]);
    }




    /**
     * Verifica que un fichero subido al servidor tenga un tipo MIME de imagen admitidos por el sistema
     * @param {String} formato: String que indica que tipo de fichero se pasa (imagen o video)
     * @param {File} fichero: Fichero subido al servidor
     * @return Boolean
     */
    static verificarTipoMime(formato,fichero) {
        var exito = true;
        const tipos = (formato!==undefined && formato==="imagen")?mimetypes:mimetypesVideos;

        console.log("fichero = " + fichero.type);
        if(tipos.mimetypes.indexOf(fichero.type)===-1) {
            exito = false;
        }
        return exito;
    }
    

}