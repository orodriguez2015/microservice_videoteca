/**
 * Clase con utilidades para el manejo de cadenas de caracteres
 * @author <a href="mailto:oscar.rodriguezbrea@gmail.com">Óscar Rodríguez</a>
 */
export class StringUtil {

    /**
     * Comprueba si una cadena de caracteres está vacía, está indefinida o es un null
     * @param {String} msg 
     */
    static isNotEmpty(msg) {
        let salida = false;
        if(msg!==null && msg!==undefined && msg.length>0 && msg.trim()!=='') {
            salida = true;
        }
        return salida;
    }


    /**
     * Recupera el ultimo elemento existente en un String después de un determinado caracter
     * @param {String} cad: Cadena de texto
     * @param {flag} flag: Flag a buscar
     * @return String
     */
    static getLastElementFromString(cad,flag) {
        var salida = null;
        if(StringUtil.isNotEmpty(cad)) {
            var datos = cad.split(flag);
            if(datos!=null && datos.length>0) {
                salida = datos[datos.length-1];
            }
        }
        return salida;
    }

}