/**
 * URL base de acceso al servidor en la que está instalado los servicios de tipo REST
 */
export const URL_BACKEND = "http://localhost:8080/videoteca";
//export const URL_BACKEND = "http://192.168.1.11:5000";

/*
 * POST /login: URL a la que se envía una petición por POST para comprobar si un usuario está autenticado
 */
export const LOGIN_API = URL_BACKEND + "/autenticar";

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
export const VIDEOTECAS_PUBLICO_API = URL_BACKEND +  "/p_videotecas";

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
 * URL utilizada para persistir, actualizar o eliminar una videoteca de la BBDD
 */
export const VIDEOTECA_API = URL_BACKEND + "/pr_videoteca";

/**
 * URL utilizada para recuperar una videoteca determinada de un determinado usuario
 */
export const VIDEOTECA_GET_API = URL_BACKEND + "/pr_videoteca/usuario";

/**
 * URL utilizada para recuperar las videotecas de un usuario
 */
export const VIDEOTECAS_API = URL_BACKEND + "/pr_videotecas";


/**
 * URL utilizada para el tratamiento de un video determinado
 */
export const VIDEO_API = URL_BACKEND + "/pr_video";

/**
 * URL para comprobar la existencia de una carpeta asociada a alguna videoteca de un determinado usuario
 */
export const VIDEOTECA_COMPRUEBA_RUTA = URL_BACKEND + "/pr_videoteca/comprobar/ruta";

/**
 * URL para comprobar la existencia de una carpeta asociada a una videoteca de un determinado usuario, pero 
 * que dicha videoteca no tenga como identificador, un determinado idVideoteca
 */
export const COMPROBAR_RUTA_OTRA_VIDEOTECA_USUARIO = URL_BACKEND + "/pr_videoteca/comprobarRutaOtraVideotecaUsuario";


export const PR_VIDEOS_API = URL_BACKEND  +  "/pr_videos/";

/**
 * Constante que representa la url del servicio rest que permite publicar/despublicar un vídeo
 */
export const PUBLICAR_VIDEO_API = URL_BACKEND + "/pr_video/publicar";


/**
 * Constante que representa la url para manejo de un recurso de tipo foto
 */
export const PR_FOTO_API = URL_BACKEND + "/pr_foto/";


/**
 * Constante que contiene la url de publicación/despublicación de una fotografía
 */
export const PUBLICAR_FOTO_API = URL_BACKEND + "/pr_foto/publicar/"


export const SUBMIT_VIDEO_API = URL_BACKEND + "/pr_videoteca/upload";