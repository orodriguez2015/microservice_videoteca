package com.oscar.videoteca.rest.manager;

import java.util.List;

import com.oscar.videoteca.rest.dto.VideotecaDTO;
import com.oscar.videoteca.rest.exception.VideotecasNotFoundException;

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
	
}
