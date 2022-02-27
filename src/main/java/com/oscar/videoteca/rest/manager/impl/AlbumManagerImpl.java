package com.oscar.videoteca.rest.manager.impl;

import java.io.File;
import java.util.List;
import java.util.Optional;

import org.apache.commons.lang3.StringUtils;
import org.apache.tomcat.util.http.fileupload.FileUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Example;
import org.springframework.data.domain.ExampleMatcher;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import com.oscar.videoteca.rest.config.BackupConfiguration;
import com.oscar.videoteca.rest.dto.AlbumDTO;
import com.oscar.videoteca.rest.dto.CreateAlbumDTO;
import com.oscar.videoteca.rest.dto.mapping.AlbumConverter;
import com.oscar.videoteca.rest.exception.AlbumNotFoundException;
import com.oscar.videoteca.rest.exception.AlbumesNotFoundException;
import com.oscar.videoteca.rest.exception.ErrorDeleteAlbumException;
import com.oscar.videoteca.rest.manager.AlbumManager;
import com.oscar.videoteca.rest.model.entity.Album;
import com.oscar.videoteca.rest.model.entity.User;
import com.oscar.videoteca.rest.model.repository.AlbumRepository;
import com.oscar.videoteca.rest.util.FileUtil;

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
	private AlbumConverter converter;
	
	@Autowired
	private BackupConfiguration backupConfiguration;
	
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
			FileUtil.createFolder(folderBackupAlbum);	
		} 	
		
		// Se da de alta el álbum en BBDD	
		Album created = albumRepository.save(converter.convertTo(album));
		
		// Se crea la subcarpeta propia del álbum en la que se almacenaránsus fotos
		String folderBackupAlbumDetail = folderBackupAlbum + File.separator + created.getId();
		FileUtil.createFolder(folderBackupAlbumDetail);

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
			String folderBackupAlbum = backupConfiguration.getAlbum();
				
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
				String folderBackupAlbumDetail = folderBackupAlbum + File.separator + id;
				
				FileUtils.deleteDirectory(new File(folderBackupAlbumDetail));
				albumRepository.delete(opt.get());
				exito = Boolean.TRUE;	
			
			}
		}catch(Exception e) {
			throw new ErrorDeleteAlbumException("Error al eliminar el álbum",e);
		}
		return exito;
		
	}


	@Override
	public AlbumDTO getAlbum(Long idAlbum, Long idUsuario) throws AlbumNotFoundException {
		ExampleMatcher publicMatcher = ExampleMatcher.matchingAll()
				  .withMatcher("id",ExampleMatcher.GenericPropertyMatchers.exact())
			      .withMatcher("idUsuarioAlta", ExampleMatcher.GenericPropertyMatchers.exact());	
		
		User user = new User();
		user.setId(idUsuario);
		
		Album a = new Album();
		a.setId(idAlbum);
		a.setUsuarioAlta(user);
		
		Example<Album> example = Example.of(a,publicMatcher);

		Optional<Album> opt = albumRepository.findOne(example);
		if(Boolean.TRUE.equals(opt.isPresent())) {
			return converter.convertTo(opt.get());
		} else {
			throw new AlbumNotFoundException("No existe el álbum fotográfico");
		}
		
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


	@Override
	public void saveFoto(MultipartFile foto, Long idAlbum, Long idUsuario) {
			
		StringBuilder path = new StringBuilder();
		path.append(backupConfiguration.getAlbum());
		path.append(File.separatorChar);
		path.append(idUsuario);
		path.append(File.separatorChar);
		path.append(idAlbum);
				
		if(Boolean.TRUE.equals(FileUtil.createFolder(path.toString()))) {
			// Si se ha creado el 
		}
		
	}

}