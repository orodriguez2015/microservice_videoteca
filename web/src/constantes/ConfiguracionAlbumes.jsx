import {URL_BACKEND} from './Configuracion.jsx';

/*
 * POST /p_album URL a la que se envía una petición por POST para grabar un álbu en el sistema
 */
export const ALBUM_SAVE_API = URL_BACKEND +  "/pr_album/save";

/*
 * POST /pr_albumes URL a la que se envía una petición por POST para grabar un álbu en el sistema
 */
export const ALBUMES_USUARIO_ADMIN_API = URL_BACKEND +  "/pr_albumes";

/*
 * GET /pr_album URL a la que se envía una petición por GET para recuperar info. básica de un álbum
 * con vista a su edición
 */
export const ALBUM_ADMIN_API = URL_BACKEND +  "/pr_album/";

/*
 * GET /p_albumes: URL a la que se envía una petición por GET para recuperar los álbumes fotográficos
 * marcados como público que se pueden mostrar en la parte pública de la web
 */
export const ALBUMES_PUBLICO_API = URL_BACKEND +  "/p_albumes";

/*
 * POST /p_album URL a la que se envía una petición por GET para recuperar las fotografías
 * que componen el álbum
 */
export const FOTO_ALBUM_PUBLICO_API = URL_BACKEND + "/p_album";


// URL para el envío de fotografías al servidor
export const URL_ATTACH_PHOTOS = URL_BACKEND + "/pr_album/adjuntar/";

/*
 * POST /pr_album URL a la que se envía una petición por GET para recuperar las fotografías
 * que componen el álbum desde la pantalla de administración de un álbum
 */
export const FOTO_ALBUM_ADMIN = URL_BACKEND + "/pr_album/";

/**
 * Constante que representa la url para manejo de un recurso de tipo foto
 */
 export const PR_FOTO_API = URL_BACKEND + "/pr_foto/";


 /**
 * Constante que contiene la url de publicación/despublicación de una fotografía
 */
export const PUBLICAR_FOTO_API = URL_BACKEND + "/pr_foto/publicar/";


