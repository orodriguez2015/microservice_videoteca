package com.oscar.videoteca.rest.manager;

import java.io.IOException;

import org.springframework.web.multipart.MultipartFile;

import com.oscar.videoteca.rest.controller.FotoDTO;
import com.oscar.videoteca.rest.exception.FotoNotFoundException;
import com.oscar.videoteca.rest.exception.SaveFileException;
import com.oscar.videoteca.rest.exception.SavePhotoException;

/**
 * Manager FotoManager
 * @author <a href="mailto:oscar.rodriguezbrea@gmail.com">Óscar Rodríguez Brea</a>
 *
 */
public interface FotoManager {
	
	
	/**
	 * Almacena las fotos asociadas a un determinado álbum fotográfico
	 * @param foto MultipartFile
	 * @throws SaveFileException si no se ha podido guardar la foto en disco
	 * @throws SavePhotoException si ocurre algún error al guarda info de la foto en BBDD
	 */
	void saveFoto(MultipartFile foto,Long idAlbum,Long idUsuario) throws IOException,SaveFileException,SavePhotoException;
	
	/**
	 * Recupera una determinada fotografía de BBDD
	 * @param idFoto
	 * @return
	 * @throws FotoNotFoundException sino se ha podido recuperar la fotografía
	 */
	FotoDTO getFoto(Long idFoto) throws FotoNotFoundException;

}
