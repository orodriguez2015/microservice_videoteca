package com.oscar.videoteca.rest.manager;

import org.springframework.web.multipart.MultipartFile;

import com.oscar.videoteca.rest.exception.SaveFileException;
import com.oscar.videoteca.rest.exception.SaveVideoException;

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
}
