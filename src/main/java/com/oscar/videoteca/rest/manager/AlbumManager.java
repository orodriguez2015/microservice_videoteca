package com.oscar.videoteca.rest.manager;

import java.util.List;

import com.oscar.videoteca.rest.dto.AlbumDTO;
import com.oscar.videoteca.rest.dto.CreateAlbumDTO;
import com.oscar.videoteca.rest.exception.AlbumNotFoundException;
import com.oscar.videoteca.rest.exception.AlbumesNotFoundException;
import com.oscar.videoteca.rest.exception.ErrorDeleteAlbumException;
import com.oscar.videoteca.rest.util.PhotoVisibilityEnum;

/**
 * Interface AlbumManager
 * @author <a href="mailto:oscar.rodriguezbrea@gmail.com">Óscar Rodríguez Brea</a>
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
	 * @throws AlbumesNotFoundException si el usuario no tiene álbumes fotográficos
	 */
	List<AlbumDTO> getAlbumesUsuario(Long idUsuario) throws AlbumesNotFoundException;
	
	
	/**
	 * Recupera un álbum fotográfico determinado de un determinado usuario
	 * @param idAlbum Id del álbum. Parámetro obligatorio
	 * @param idUsuario Id del usuario. Puede estar a nulo, por tanto sólo se filtraría por el parámetro idAlbum
	 * @param visibility PhotoVisibilityEnum
	 * @return AlbumDTO
	 * @throws AlbumNotFoundException si no se ha podido recuperar el álbum
	 */
	AlbumDTO getAlbum(Long idAlbum,Long idUsuario,PhotoVisibilityEnum visibility) throws AlbumNotFoundException;
	
	
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
	 * @throws ErrorDeleteAlbumException si ocurre algún error durante el borrado
	 */
	Boolean deleteAlbum(Long id,Long idUsuario) throws ErrorDeleteAlbumException;
	
	
	/**
	 * Permite actualizar un álbum fotográfico determinada
	 * @param album CreateAlbumDTO
	 * @return AlbumDTO modificado
	 * @throws AlbumNotFoundException si no existe el álbum a modificar
	 */
	AlbumDTO updateAlbum(CreateAlbumDTO album) throws AlbumNotFoundException;
	
	/**
	 * Comprueba si existe un álbum en la BBDD
	 * @param id Long
	 * @return Boolean
	 */
	Boolean existsById(Long id);
	
}
