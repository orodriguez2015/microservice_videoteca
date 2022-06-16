import {URL_BACKEND} from './Configuracion.jsx';

/*
 * POST /p_album URL a la que se envía una petición por POST para grabar un álbu en el sistema
 */
export const ALBUM_PRIVATE_API = URL_BACKEND +  "/private/album";

/*
 * POST /private/albumes/ URL a la que se envía una petición por POST para grabar un álbu en el sistema
 */
export const ALBUMES_USUARIO_ADMIN_API = URL_BACKEND +  "/private/albumes/";


/*
 * GETT /private/album/detail URL a la que se envía una petición para recuperar el detalle de un álbum fotográfico de un usuario
 */

export const ALBUM_DETAIL_API = URL_BACKEND + "/private/album/detail/";

/*
 * GET /pr_album URL a la que se envía una petición por GET para recuperar info. básica de un álbum
 * con vista a su edición
 */
export const ALBUM_ADMIN_API = URL_BACKEND +  "/pr_album/";

/*
 * GET /p_albumes: URL a la que se envía una petición por GET para recuperar los álbumes fotográficos
 * marcados como público que se pueden mostrar en la parte pública de la web
 */
export const ALBUMES_PUBLICO_API = URL_BACKEND +  "/public/albumes";


export const ALBUM_PUBLICO_DETAIL_API = URL_BACKEND + "/public/album/detail/";

// URL para el envío de fotografías al servidor
export const URL_ATTACH_PHOTOS = URL_BACKEND + "/pr_album/adjuntar/";


/**
 * Constante que representa la url para manejo de un recurso de tipo foto
 */
 export const PR_FOTO_API = URL_BACKEND + "/pr_foto/";


 /**
 * Constante que contiene la url de publicación/despublicación de una fotografía
 */
export const PUBLICAR_FOTO_API = URL_BACKEND + "/private/photo/publish/";


/**
 * Constante que representa la url para manejo de un recurso de tipo foto desde la pantalla de administración
 */
 export const PRIVATE_PHOTO_API = URL_BACKEND + "/private/photo/";



