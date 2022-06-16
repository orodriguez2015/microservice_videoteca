package com.oscar.videoteca.rest.manager.impl;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Example;
import org.springframework.data.domain.ExampleMatcher;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import com.oscar.videoteca.rest.dto.PhotoDTO;
import com.oscar.videoteca.rest.dto.mapping.PhotoConverter;
import com.oscar.videoteca.rest.exception.ErrorDeletePhotoException;
import com.oscar.videoteca.rest.exception.ErrorPublishPhotoException;
import com.oscar.videoteca.rest.exception.PhotoNotFoundException;
import com.oscar.videoteca.rest.exception.SaveFileException;
import com.oscar.videoteca.rest.exception.SavePhotoException;
import com.oscar.videoteca.rest.manager.PhotoManager;
import com.oscar.videoteca.rest.model.entity.Album;
import com.oscar.videoteca.rest.model.entity.Photo;
import com.oscar.videoteca.rest.model.entity.User;
import com.oscar.videoteca.rest.model.repository.PhotoRepository;
import com.oscar.videoteca.rest.util.FileUtil;
import com.oscar.videoteca.rest.util.PhotoVisibilityEnum;
import com.oscar.videoteca.rest.util.PhotoVisibilityFactory;

/**
 * Clase FotoManagerImpl
 * @author <a href="mailto:oscar.rodriguezbrea@gmail.com">Óscar Rodríguez Brea</a>
 *
 */
@Service
public class PhotoManagerImpl implements PhotoManager {

	@Autowired
	private PhotoRepository photoRepository;
		
	@Autowired
	private PhotoConverter fotoConverter;
	
	@Autowired
	private FileUtil fileUtil;

	@Override
	public void savePhoto(MultipartFile foto, Long idAlbum, Long idUsuario) throws IOException,SaveFileException, SavePhotoException {
		
		Boolean userPathCreated = fileUtil.createFolder(fileUtil.getBackupUserDirectory(idUsuario));		
		Boolean albumPathCreated = fileUtil.createFolder(fileUtil.getBackupAlbumDirectory(idAlbum, idUsuario));
				
		if(Boolean.TRUE.equals(albumPathCreated) && Boolean.TRUE.equals(userPathCreated)) {
			// Si se ha creado el directorio o ya existe, se persiste la foto			
			
			String path = fileUtil.getBackupPhoto(idAlbum, idUsuario,foto.getOriginalFilename());
			
			Boolean continuar = Boolean.FALSE;
			try {
				fileUtil.saveFile(foto.getInputStream(),path);
					
				continuar =Boolean.TRUE;
			}catch(SaveFileException e) {
				throw e;
			}
			
			if(Boolean.TRUE.equals(continuar)) {
				// Si se ha almacenado el fichero en disco => Se almacena registro en BBDD
				User user = new User();
				user.setId(idUsuario);
				
				Album album = new Album();
				album.setId(idAlbum);
				
				Photo f = new Photo();
				f.setNombre(foto.getOriginalFilename());
				f.setTipoMime(foto.getContentType());
				f.setRuta(path);
				f.setRutaRelativa(fileUtil.getRelativePathPhoto(foto.getOriginalFilename(), idAlbum, idUsuario));
				f.setPublico(Boolean.TRUE);
		
				File fotografia = new File(path);
				
				FileUtil.PhotoSize size = fileUtil.getPhotoSize(fotografia);
				f.setAlto(size.getHeight());
				f.setAncho(size.getWidth());
				
				f.setNumeroVisualizaciones(Integer.valueOf(0));			
				f.setUsuario(user);
				f.setAlbum(album);
				try {
					photoRepository.saveAndFlush(f);
				}catch(Exception e) {
					e.printStackTrace();
					
					// Si ha ocurrido alǵun error, entonces se borra la foto del disco
					fileUtil.deleteFile(fotografia);
					
					throw new SavePhotoException(e.getMessage());
				}
			}
		}
		
	}

	@Override
	public PhotoDTO getPhoto(Long idFoto) throws PhotoNotFoundException {
		PhotoDTO foto = null;
		
		if(Boolean.TRUE.equals(photoRepository.existsById(idFoto))){
			Optional<Photo> opt = photoRepository.findById(idFoto);
			if(opt.isPresent()) {
				foto = fotoConverter.convertTo(opt.get());
			}
			
		} else {
			throw new PhotoNotFoundException("No existe la fotografía");
		}
		return foto;
	}

	@Override
	public Boolean deletePhoto(Long idPhoto, Long idUsuario) throws PhotoNotFoundException, ErrorDeletePhotoException {
		Boolean exito  = Boolean.FALSE;
		try {
			ExampleMatcher exampleMatcher = ExampleMatcher.matchingAll()
				      .withMatcher("id", ExampleMatcher.GenericPropertyMatchers.exact().ignoreCase())
				      .withMatcher("idUsuarioAlta", ExampleMatcher.GenericPropertyMatchers.exact().ignoreCase());
	
			Photo photo = new Photo();
			photo.setId(idPhoto);
			
			User user = new User();
			user.setId(idUsuario);		
			photo.setUsuario(user);
			
			Example<Photo> example = Example.of(photo,exampleMatcher);		
			Optional<Photo> opt = photoRepository.findOne(example);
			
			if(opt.isPresent()) {
				// Se elimina el fichero del disco y, a posteriori, se elimina de la BBDD	
				File file = new File(opt.get().getRuta());
				
				if(Boolean.TRUE.equals(fileUtil.deleteFile(file))) {
					photoRepository.delete(opt.get());
					exito = Boolean.TRUE;	
				}	
			}
		}catch(Exception e) {
			throw new ErrorDeletePhotoException("Error al eliminar la fotografía",e);
		}
		return exito;
		
	}

	@Override
	public Boolean publishPhoto(Long idPhoto, Long idUser, Long value) throws ErrorPublishPhotoException, PhotoNotFoundException {
		Boolean exito = Boolean.FALSE;
		
		Photo p = this.getPhoto(idPhoto, idUser);
		if(p==null) {
			throw new PhotoNotFoundException("No se ha encontrado la fotografía");
		}
		
		try {
			p.setPublico(Boolean.FALSE);
			if(value.equals(1L)) {
				p.setPublico(Boolean.TRUE);
			}
			
			// Se actualiza la fotografía
			this.photoRepository.saveAndFlush(p);
			exito =Boolean.TRUE;
		}catch(Exception e) {
			throw new ErrorPublishPhotoException("Se ha producido un error al publicar una fotografía",e);
		}
		
		return exito;
		
	}

	@Override
	public Photo getPhoto(Long idPhoto, Long idUser) throws PhotoNotFoundException {
		ExampleMatcher exampleMatcher = ExampleMatcher.matchingAll()
			      .withMatcher("id", ExampleMatcher.GenericPropertyMatchers.exact().ignoreCase())
			      .withMatcher("idUsuarioAlta", ExampleMatcher.GenericPropertyMatchers.exact().ignoreCase());
		
		Photo p = new Photo();
		p.setId( idPhoto);
	
		User user = new User();
		user.setId(idUser);
		p.setUsuario(user);
		
		Example<Photo> example = Example.of(p,exampleMatcher);
		Optional<Photo> opt = this.photoRepository.findOne(example);
		if(Boolean.FALSE.equals(opt.isPresent())) {
			throw new PhotoNotFoundException("No existe la fotografía"); 
		} else {
			p = opt.get();
		}
		
		return p;
			
		
	}

	@Override
	public List<Photo> getPhotos(Long idAlbum, PhotoVisibilityEnum visibility) throws PhotoNotFoundException {
		
		ExampleMatcher photoMatcher = ExampleMatcher.matchingAll()
			      .withMatcher("idAlbum", ExampleMatcher.GenericPropertyMatchers.exact().ignoreCase());
			      
		if(!visibility.equals(PhotoVisibilityEnum.ALL_PHOTOS))  {	      
			photoMatcher.withMatcher("publico", ExampleMatcher.GenericPropertyMatchers.exact().ignoreCase());
		}
		
		Album album = new Album();
		album.setId(idAlbum);
		
		// Se buscan las fotos de un determinado álbum y con un determinado tipo de visibilidad
		Photo photo = new Photo();
		photo.setAlbum(album);
		
		// Se establece la visibilidad de la fotografía
		PhotoVisibilityFactory.establishVisibilityPhotography(photo, visibility);
		
		Example<Photo> example = Example.of(photo,photoMatcher);
		return photoRepository.findAll(example);
	}

}
