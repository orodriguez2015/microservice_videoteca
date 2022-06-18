/** 
 * Constante que contiene el estado inicial de la aplicación, que inicialmente
 * sólo contiene el objeto usuario, que alberga los datos básicos de la cuenta del usuario recuperados
 * de la base de datos
 */
export const ESTADO_USUARIO_INICIAL = {
    id: -1,
    nombre: '',
    apellido1: '',
    apellido2: '',
    email: '',
    login:'',
    admin:''
}


/** Estado de publicación de una fotografía */
export const ESTADO_PUBLICACION_FOTO    = "1";
/** Estado de despublicación de una fotografía */
export const ESTADO_DESPUBLICACION_FOTO = "0";
/** Constante ELEMENT */
export const ELEMENT = "element";