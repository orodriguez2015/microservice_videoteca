package com.oscar.videoteca.rest.manager;

import java.io.IOException;

import org.springframework.web.multipart.MultipartFile;

import com.oscar.videoteca.rest.exception.SaveFileException;

/**
 * Manager FotoManager
 * @author <a href="mailto:oscar.rodriguezbrea@gmail.com">Óscar Rodríguez Brea</a>
 *
 */
public interface FotoManager {
	
	
	/**
	 * Almacena las fotos asociadas a un determinado álbum fotográfico
	 * @param foto MultipartFile
	 * @throws SaveFileException si ocurre algún error 
	 */
	void saveFoto(MultipartFile foto,Long idAlbum,Long idUsuario) throws IOException,SaveFileException;

}
