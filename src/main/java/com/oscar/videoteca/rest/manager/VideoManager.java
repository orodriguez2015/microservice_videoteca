package com.oscar.videoteca.rest.manager;

import org.springframework.web.multipart.MultipartFile;

import com.oscar.videoteca.rest.exception.ErrorDeleteVideoException;
import com.oscar.videoteca.rest.exception.SaveFileException;
import com.oscar.videoteca.rest.exception.SaveVideoException;
import com.oscar.videoteca.rest.exception.VideoNotFoundException;
import com.oscar.videoteca.rest.model.entity.Video;

/**
 * Interface VideoManager
 * @author <a href="mailto:oscar.rodriguezbrea@gmail.com">Óscar Rodríguez</a>
 *
 */
public interface VideoManager {

	/**
	 * Persiste un vídeo subido al servidor en disco y queda asociado a la videoteca 
	 * @param foto MultipartFile
	 * @param idVideoteca Id de la videoteca
	 * @param idUsuario Id del usuario
	 * @throws SaveFileException
	 */
	void saveVideo(MultipartFile foto, Long idVideoteca, Long idUsuario)  throws SaveVideoException;
	
	
	/**
	 * Recupera un determinado vídeo de un usuario
	 * @param idVideo Id del vídeo
	 * @param idUsuario Id del usuairo
	 * @return Video
	 * @throws VideoNotFoundException sino existe el vídeo
	 * 
	 */
	Video getVideo(Long idVideo,Long idUsuario) throws VideoNotFoundException;
	
	/**
	 * Permite marca un vídeo como público o no. Solo los vídeos públicos serán visualizados en el área pública de la web
	 * @param idVideo Id del vídeo
	 * @param idUsuario Id del usuario
	 * @param value 1 si se publica y 0 si se despublica
	 * @return True si se ha publicado y false en caso contrario
	 * @throws VideoNotFoundException si no existe el vídeo
	 */
	Boolean publishVideo(Long idVideo,Long idUsuario,Long value) throws VideoNotFoundException;
	
	
	/**
	 * Eliminar un vídeo de BBDD y del almacenamiento en disco
	 * @param idVideo Id del vídeo
	 * @param idUsuario Id del usuario
	 * @return True si se ha eliminado y false en caso contrario
	 * @throws ErrorDeleteVideoException si ocurre algún error
	 */
	Boolean deleteVideo(Long idVideo,Long idUsuario) throws ErrorDeleteVideoException;
	
}
