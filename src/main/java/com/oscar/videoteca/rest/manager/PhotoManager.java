package com.oscar.videoteca.rest.manager;

import java.io.IOException;

import org.springframework.web.multipart.MultipartFile;

import com.oscar.videoteca.rest.dto.FotoDTO;
import com.oscar.videoteca.rest.exception.ErrorDeletePhotoException;
import com.oscar.videoteca.rest.exception.PhotoNotFoundException;
import com.oscar.videoteca.rest.exception.SaveFileException;
import com.oscar.videoteca.rest.exception.SavePhotoException;

/**
 * Manager PhotoManager
 * @author <a href="mailto:oscar.rodriguezbrea@gmail.com">Óscar Rodríguez Brea</a>
 *
 */
public interface PhotoManager {
	
	/**
	 * Almacena las fotos asociadas a un determinado álbum fotográfico
	 * @param foto MultipartFile
	 * @throws SaveFileException si no se ha podido guardar la foto en disco
	 * @throws SavePhotoException si ocurre algún error al guarda info de la foto en BBDD
	 */
	void savePhoto(MultipartFile foto,Long idAlbum,Long idUsuario) throws IOException,SaveFileException,SavePhotoException;
	
	/**
	 * Recupera una determinada fotografía de BBDD
	 * @param idFoto
	 * @return
	 * @throws PhotoNotFoundException sino se ha podido recuperar la fotografía
	 */
	FotoDTO getPhoto(Long idFoto) throws PhotoNotFoundException;
	
	
	/**
	 * Elimina una fotografía fisicamente de la BBDD y del disco
	 * @param idPhoto Id de la fotografía
	 * @param idUsuario Id del usuario
	 * @return True si se ha eliminado y false en caso contrario
	 * @throws PhotoNotFoundException Si no existe la fotografía en BBDD
	 * @throws ErrorDeletePhotoException Si no se ha podido eliminar la fotografía
	 */
	Boolean deletePhoto(Long idPhoto,Long idUsuario) throws PhotoNotFoundException,ErrorDeletePhotoException;

}
