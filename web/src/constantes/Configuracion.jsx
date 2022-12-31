/**
 * URL base de acceso al servidor en la que está instalado los servicios de tipo REST
 */
export const URL_BACKEND = "http://localhost:8080/videoteca";

/** URL para mostrar los recursos en forma de imágenes */
export const URL_BACKEND_IMAGES = URL_BACKEND + "/images";


/** URL para mostrar los recursos en forma de vídeos */
export const URL_BACKEND_VIDEOS = URL_BACKEND + "/videos";

/*
 * POST /login: URL a la que se envía una petición por POST para comprobar si un usuario está autenticado
 */
export const LOGIN_API = URL_BACKEND + "/login";

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


/*
 * POST /pr_album URL a la que se envía una petición por GET para recuperar las fotografías
 * que componen el álbum desde la pantalla de administración de un álbum
 */

export const FOTO_ALBUM_ADMIN = URL_BACKEND + "/pr_album/";

/*
 * GET /p_videotecas URL a la que se envía una petición por GET para recuperar las videotecas públicas
 */
export const VIDEOTECAS_PUBLICO_API = URL_BACKEND +  "/public/videotecas";

/*
 * GET /p_videos URL a la que se envía una petición por GET para recuperar las vídeos de una videoteca
 */
export const VIDEOS_VIDEOTECA_PUBLICO_API = URL_BACKEND +  "/p_videos";

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


/**
 * URL utilizada para recuperar una videoteca determinada de un determinado usuario
 */
export const VIDEOTECA_GET_API = URL_BACKEND + "/pr_videoteca/usuario";

/**
 * URL utilizada para recuperar las videotecas de un usuario
 */
export const VIDEOTECAS_API = URL_BACKEND + "/private/videotecas";


/**
 * URL utilizada para el tratamiento de un video determinado
 */
export const VIDEO_API = URL_BACKEND + "/pr_video";


export const PR_VIDEOS_API = URL_BACKEND  +  "/pr_videos/";

/**
 * Constante que representa la url del servicio rest que permite publicar/despublicar un vídeo
 */
export const PUBLICAR_VIDEO_API = URL_BACKEND + "/pr_video/publicar";


/**
 * Constante que contiene la url de publicación/despublicación de una fotografía
 */
export const PUBLICAR_FOTO_API = URL_BACKEND + "/pr_foto/publicar/"


/**
 * Constante que contiene la url de upload de un vídeo al servidor
 */
export const SUBMIT_VIDEO_API = URL_BACKEND + "/private/video/";


/**
 * Constante que contiene la url que permite recuperar vídeos de una videoteca
 */
export const GET_VIDEOS_API = URL_BACKEND + "/private/videos/";



