package com.oscar.videoteca.rest.manager.impl;

import java.io.File;
import java.util.List;
import java.util.Optional;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Example;
import org.springframework.data.domain.ExampleMatcher;
import org.springframework.stereotype.Service;

import com.oscar.videoteca.rest.config.BackupConfiguration;
import com.oscar.videoteca.rest.dto.AlbumDTO;
import com.oscar.videoteca.rest.dto.CreateAlbumDTO;
import com.oscar.videoteca.rest.dto.mapping.AlbumConverter;
import com.oscar.videoteca.rest.dto.mapping.PhotoConverter;
import com.oscar.videoteca.rest.exception.AlbumNotFoundException;
import com.oscar.videoteca.rest.exception.AlbumesNotFoundException;
import com.oscar.videoteca.rest.exception.ErrorDeleteAlbumException;
import com.oscar.videoteca.rest.exception.ErrorGetPhotosAlbumException;
import com.oscar.videoteca.rest.exception.PhotoNotFoundException;
import com.oscar.videoteca.rest.manager.AlbumManager;
import com.oscar.videoteca.rest.manager.PhotoManager;
import com.oscar.videoteca.rest.model.entity.Album;
import com.oscar.videoteca.rest.model.entity.Photo;
import com.oscar.videoteca.rest.model.entity.User;
import com.oscar.videoteca.rest.model.repository.AlbumRepository;
import com.oscar.videoteca.rest.util.FileUtil;
import com.oscar.videoteca.rest.util.ResourceVisibilityEnum;

/**
 * Implementación de AlbumManager
 * @author <a href="mailto:oscar.rodriguezbrea@gmail.com">Óscar Rodríguez Brea</a>
 *
 */
@Service
public class AlbumManagerImpl implements AlbumManager {

	@Autowired
	private AlbumRepository albumRepository;
	
	@Autowired
	private PhotoManager photoManager;
	
	@Autowired
	private AlbumConverter converter;
	
	@Autowired
	private PhotoConverter photoConverter;
	
	
	@Autowired
	private BackupConfiguration backupConfiguration;
	
	@Autowired
	private FileUtil fileUtil;
	
	@Override
	public List<AlbumDTO> getAlbumesPublicos() {
	
		ExampleMatcher publicMatcher = ExampleMatcher.matchingAll()
			      .withMatcher("publico", ExampleMatcher.GenericPropertyMatchers.exact());	
		
		Album a = new Album();
		a.setPublico(Boolean.TRUE);
		
		Example<Album> example = Example.of(a,publicMatcher);
		
		List<Album> albumes = albumRepository.findAll(example);
		return converter.converTo(albumes);
	}

	
	@Override
	public List<AlbumDTO> getAlbumesUsuario(Long idUsuario) throws AlbumesNotFoundException {
		
		ExampleMatcher publicMatcher = ExampleMatcher.matchingAll()
			      .withMatcher("idUsuarioAlta", ExampleMatcher.GenericPropertyMatchers.exact());	
		
		User user = new User();
		user.setId(idUsuario);
		
		Album a = new Album();
		a.setUsuarioAlta(user);
		
		Example<Album> example = Example.of(a,publicMatcher);
		
		List<Album> albumes = albumRepository.findAll(example);
		if(albumes==null || albumes.isEmpty()) {
			throw new AlbumesNotFoundException("No hay álbumes fotográficos para el usuario actual");
		}
		
		return converter.converTo(albumes);
	}


	@Override
	public AlbumDTO saveAlbum(CreateAlbumDTO album) {
		AlbumDTO salida = null;
		String folderBackupAlbum = backupConfiguration.getAlbum();
		if(StringUtils.isNotEmpty(folderBackupAlbum)) {
			// Se crea la carpeta raíz de backup de álbumes sino existe
			fileUtil.createFolder(folderBackupAlbum);	
		} 	
		
		// Se da de alta el álbum en BBDD	
		Album created = albumRepository.save(converter.convertTo(album));
		
		// Se crea la subcarpeta propia del álbum en la que se almacenaránsus fotos
		String folderBackupAlbumDetail = folderBackupAlbum + File.separator + created.getId();
		fileUtil.createFolder(folderBackupAlbumDetail);

		salida = converter.convertTo(created); 
	
				
		return salida;
	}
	
	
	/**
	 * Comprueba si existe un álbum en la BBDD
	 * @param id Long
	 * @return Boolean
	 */
	@Override
	public Boolean existsById(Long id) {
		return albumRepository.existsById(id);
	}


	@Override
	public Boolean deleteAlbum(Long id,Long idUsuario) throws ErrorDeleteAlbumException {
		Boolean exito  = Boolean.FALSE;
		
		try {
				
			ExampleMatcher exampleMatcher = ExampleMatcher.matchingAll()
				      .withMatcher("id", ExampleMatcher.GenericPropertyMatchers.exact().ignoreCase())
				      .withMatcher("idUsuarioAlta", ExampleMatcher.GenericPropertyMatchers.exact().ignoreCase());
	
			Album album = new Album();
			album.setId(id);
			
			User user = new User();
			user.setId(idUsuario);		
			album.setUsuarioAlta(user);
			
			
			Example<Album> example = Example.of(album,exampleMatcher);		
			Optional<Album> opt = albumRepository.findOne(example);
			
			if(opt.isPresent()) {
					
				// Se elimina la subcarpeta que contiene las fotos del álbumm
				fileUtil.deleteDirectory(new File(fileUtil.getBackupAlbumDirectory(id, idUsuario)));
				albumRepository.delete(opt.get());
				exito = Boolean.TRUE;	
			
			}
		}catch(Exception e) {
			throw new ErrorDeleteAlbumException("Error al eliminar el álbum",e);
		}
		return exito;
		
	}


	@Override
	public AlbumDTO getAlbum(Long idAlbum, Long idUsuario,ResourceVisibilityEnum visibility) throws AlbumNotFoundException, ErrorGetPhotosAlbumException {
		AlbumDTO album = null;
		ExampleMatcher publicMatcher = ExampleMatcher.matchingAll()
				  .withMatcher("id",ExampleMatcher.GenericPropertyMatchers.exact());
		
		if(idUsuario!=null) {
			publicMatcher.withMatcher("idUsuarioAlta", ExampleMatcher.GenericPropertyMatchers.exact());
		}
				
		Album a = new Album();
		a.setId(idAlbum);
		
		if(idUsuario!=null) {
			User user = new User();
			user.setId(idUsuario);
			a.setUsuarioAlta(user);		
		}

		Example<Album> example = Example.of(a,publicMatcher);
		Optional<Album> opt = albumRepository.findOne(example);
		
		
		if(Boolean.FALSE.equals(opt.isPresent())) {
			throw new AlbumNotFoundException("No existe el álbum fotográfico");
		} else {
			// Se recuperan las fotografías del album que tienen una determinada visibilidad
			try {
				
				/** Se recuperan las fotos pública del álbum */
				List<Photo> photos = photoManager.getPhotos(idAlbum,visibility);
				
				album = converter.convertTo(opt.get());
				album.setFotos(photoConverter.convertTo(photos));
								
			}catch(PhotoNotFoundException e) {
				throw new ErrorGetPhotosAlbumException("Error al recuperar fotos de un álbum");
			}	
		}
		return album;
		
	}


	@Override
	public AlbumDTO updateAlbum(CreateAlbumDTO album) {		
		Optional<Album> opt = albumRepository.findById(album.getId());
		AlbumDTO nuevo = null;
		if(Boolean.FALSE.equals(opt.isPresent())) {
			throw new AlbumNotFoundException("No existe el álbum que se pretende modificar");
		} else {
			Album original = opt.get();
			original.setDescripcion(album.getDescripcion());
			original.setNombre(album.getNombre());
			original.setPublico(album.getPublico());
			nuevo = converter.convertTo(albumRepository.save(original));
			
		}
		return nuevo;
	}



}