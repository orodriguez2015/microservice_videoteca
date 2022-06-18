package com.oscar.videoteca.rest.manager;

import java.io.IOException;
import java.util.List;

import org.springframework.web.multipart.MultipartFile;

import com.oscar.videoteca.rest.dto.PhotoDTO;
import com.oscar.videoteca.rest.exception.ErrorDeletePhotoException;
import com.oscar.videoteca.rest.exception.ErrorPublishPhotoException;
import com.oscar.videoteca.rest.exception.PhotoNotFoundException;
import com.oscar.videoteca.rest.exception.SaveFileException;
import com.oscar.videoteca.rest.exception.SavePhotoException;
import com.oscar.videoteca.rest.model.entity.Photo;
import com.oscar.videoteca.rest.util.PhotoVisibilityEnum;

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
	PhotoDTO getPhoto(Long idFoto) throws PhotoNotFoundException;
	
	
	/**
	 * Recupera una determinada fotografía de BBDD dada de alta por un determinado usuario
	 * @param idFoto Id de la fotografía
	 * @param idUser Id del usuario 
	 * @return Photo
	 * @throws PhotoNotFoundException sino se ha podido recuperar la fotografía
	 */
	Photo getPhoto(Long idFoto,Long idUser) throws PhotoNotFoundException;
	
	
	/**
	 * Elimina una fotografía fisicamente de la BBDD y del disco
	 * @param idPhoto Id de la fotografía
	 * @param idUsuario Id del usuario
	 * @return True si se ha eliminado y false en caso contrario
	 * @throws PhotoNotFoundException Si no existe la fotografía en BBDD
	 * @throws ErrorDeletePhotoException Si no se ha podido eliminar la fotografía
	 */
	Boolean deletePhoto(Long idPhoto,Long idUsuario) throws PhotoNotFoundException,ErrorDeletePhotoException;
	
	
	/**
	 * Permite publicar/despublicar una determinada fotografía
	 * @param idPhoto Id de la foto
	 * @param idUser Id del usuario
	 * @param value 1 si se publica y 0 en caso contrario
	 * @return True si se ha cambiado el valor de publicación de la fotografía
	 * @throws ErrorPublishPhotoException si ocurre algún error al publicar
	 *         PhotoNotFoundException si no se ha podido recuperar la fotografía
	 */
	Boolean publishPhoto(Long idPhoto,Long idUser,Long value) throws ErrorPublishPhotoException,PhotoNotFoundException;
	
	/**
	 * Recupera las fotografías de un album
	 * @param idAlbum Id del álbum
	 * @param visibility Visibilidad de las fotografías
	 * @return List<Photo>
	 * @throws PhotoNotFoundException si ocurre algún error
	 */
	List<Photo> getPhotos(Long idAlbum,PhotoVisibilityEnum visibility) throws PhotoNotFoundException; 
	
	/**
	 * Incrementa el contador de visualización de una fotografía
	 * @param idPhoto Long
	 * @return Boolean
	 * @throws PhotoNotFoundException si ocurre algún error
	 */
	Boolean increasePhotoDisplayCounter(Long idPhoto) throws PhotoNotFoundException;
}