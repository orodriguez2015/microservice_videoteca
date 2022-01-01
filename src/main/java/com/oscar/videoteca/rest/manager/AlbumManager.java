package com.oscar.videoteca.rest.manager;

import java.util.List;

import com.oscar.videoteca.rest.dto.AlbumDTO;
import com.oscar.videoteca.rest.dto.CreateAlbumDTO;

/**
 * Interface AlbumManager
 * @author <a href="mailto:oscarrbr@ext.inditex.com">Óscar Rodríguez Brea</a>
 *
 */
public interface AlbumManager {

	/**
	 * Recupera los álbumes fotográficos públicos
	 * @return List<AlbumDTO>
	 */
	List<AlbumDTO> getAlbumesPublicos();
	
	
	/**
	 * Recupera los álbumes fotográficos de un determinado usuario
	 * @param idUsuario Long
	 * @return List<AlbumDTO>
	 */
	List<AlbumDTO> getAlbumesUsuario(Long idUsuario);
	
	/**
	 * Persiste un álbum fotográfico en BBDD
	 * @param album CreateAlbumDTO con la info básica del álbum a crear
	 * @return AlbumDTO con los datos del álbum creado más completos
	 */
	AlbumDTO saveAlbum(CreateAlbumDTO album);
	
	/**
	 * Elimina un álbum de BBDD que pertenezca a un determinado usuario
	 * @param id Id del álbum a eliminar
	 * @param idUsuario: Id del usuario al que pertenece el álbum
	 * @return True si se ha podido borrar y false en caso contrario
	 */
	Boolean deleteAlbum(Long id,Long idUsuario);
	
	/**
	 * Comprueba si existe un álbum en la BBDD
	 * @param id Long
	 * @return Boolean
	 */
	Boolean existsById(Long id);
}
