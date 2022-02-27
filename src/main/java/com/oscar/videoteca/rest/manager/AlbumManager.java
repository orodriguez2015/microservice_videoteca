package com.oscar.videoteca.rest.manager;

import java.io.IOException;
import java.util.List;

import org.springframework.web.multipart.MultipartFile;

import com.oscar.videoteca.rest.dto.AlbumDTO;
import com.oscar.videoteca.rest.dto.CreateAlbumDTO;
import com.oscar.videoteca.rest.exception.AlbumNotFoundException;
import com.oscar.videoteca.rest.exception.AlbumesNotFoundException;
import com.oscar.videoteca.rest.exception.ErrorDeleteAlbumException;
import com.oscar.videoteca.rest.exception.SaveFileException;

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
	 * @param idAlbum Id del álbum
	 * @param idUsuario Id del usuario
	 * @return AlbumDTO
	 * @throws AlbumNotFoundException
	 */
	AlbumDTO getAlbum(Long idAlbum,Long idUsuario) throws AlbumNotFoundException;
	
	
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
	
	/**
	 * Almacena las fotos asociadas a un determinado álbum fotográfico
	 * @param foto MultipartFile
	 * @throws SaveFileException si ocurre algún error 
	 */
	void saveFoto(MultipartFile foto,Long idAlbum,Long idUsuario) throws IOException,SaveFileException;
}
