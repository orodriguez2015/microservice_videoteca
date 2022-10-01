package com.oscar.videoteca.rest.manager;

import java.util.List;

import com.oscar.videoteca.rest.dto.CreateVideotecaDTO;
import com.oscar.videoteca.rest.dto.VideotecaDTO;
import com.oscar.videoteca.rest.exception.ErrorDeleteVideotecaException;
import com.oscar.videoteca.rest.exception.VideotecaNotFoundException;
import com.oscar.videoteca.rest.exception.VideotecasNotFoundException;
import com.oscar.videoteca.rest.util.ResourceVisibilityEnum;

/**
 * VideotecaManager
 * @author <a href="mailto:oscar.rodriguezbrea@gmail.com">Óscar Rodríguez Brea</a>
 *
 */
public interface VideotecaManager {

	/**
	 * Recupera un listado de las videotecas públicas existentes en la BBDD
	 * @return List<VideotecaDTO>
	 */
	List<VideotecaDTO> getVideotecasPublicas();
	
	/**
	 * Recupera las videotecas de un determinado usuario
	 * @param id Id del usuario
	 * @return List<VideotecaDTO>
	 * @throws VideotecasNotFoundException sino hay videotecas 
	 */
	List<VideotecaDTO> getVideotecasUsuario(Long id) throws VideotecasNotFoundException;
	
	/**
	 * Recupera una videoteca determinada
	 * @param id Id de la videoteca
	 * @param idUsuario Id del usuario
	 * @param visibility ResourceVisibilityEnum que hace referencia a si se recupera los vídeos públicos, privados, todos o bien ninguno
	 * @return VideotecaDTO
	 * @throws VideotecaNotFoundException si ocurre algún error
	 */
	VideotecaDTO getVideoteca(Long id,Long idUsuario,ResourceVisibilityEnum visibility) throws VideotecaNotFoundException;
	
	/**
	 * Comprueba si un determinado usuario ya tiene una videoteca asociada a una determinada carpeta
	 * @param idUsuario Long
	 * @param folder String
	 * @return True si existe y false en caso contrario
	 */
	Boolean checkVideoteca(Long idUsuario,String folder);
	
	/**
	 * Persiste una nueva videoteca en BBDD
	 * @param create CreateVideotecaDTO
	 * @return VideotecaDTO creada
	 */
	VideotecaDTO save(CreateVideotecaDTO create);
	
	
	/**
	 * Actualiza una determinada videoteca
	 * @param update CreateVideotecaDTO
	 * @return VideotecaDTO
	 * @throws VideotecaNotFoundException si no se ha podido recuperar la videoteca
	 */
	VideotecaDTO update(CreateVideotecaDTO update) throws VideotecaNotFoundException;
	
	/**
	 * Borra una videoteca
	 * @param id Id de la videoteca
	 * @param idUsuario Id del usuario
	 * @return Boolean
	 * @throws ErrorDeleteVideotecaException
	 */
	Boolean delete(Long id,Long idUsuario) throws ErrorDeleteVideotecaException; 
	
	/**
	 * Comprueba si existe una videoteca con un determinado id
	 * @param id Long
	 * @return Boolean
	 */
	Boolean existsById(Long id);
	
}
