package com.oscar.videoteca.rest.manager;

import java.util.List;

import com.oscar.videoteca.rest.dto.VideotecaDTO;

/**
 * Convierte
 * @author <a href="mailto:oscarrbr@ext.inditex.com">Óscar Rodríguez Brea</a>
 *
 */
public interface VideotecaManager {

	/**
	 * Recupera un listado de las videotecas públicas existentes en la BBDD
	 * @return
	 */
	List<VideotecaDTO> getVideotecasPublicas();
	
}
