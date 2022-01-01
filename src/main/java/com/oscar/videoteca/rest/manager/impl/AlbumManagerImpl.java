package com.oscar.videoteca.rest.manager.impl;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.Optional;

import org.apache.commons.lang3.StringUtils;
import org.apache.tomcat.util.http.fileupload.FileUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Example;
import org.springframework.data.domain.ExampleMatcher;
import org.springframework.stereotype.Service;

import com.oscar.videoteca.rest.config.BackupConfiguration;
import com.oscar.videoteca.rest.dto.AlbumDTO;
import com.oscar.videoteca.rest.dto.CreateAlbumDTO;
import com.oscar.videoteca.rest.dto.mapping.AlbumConverter;
import com.oscar.videoteca.rest.manager.AlbumManager;
import com.oscar.videoteca.rest.model.entity.Album;
import com.oscar.videoteca.rest.model.entity.User;
import com.oscar.videoteca.rest.model.repository.AlbumRepository;
import com.oscar.videoteca.rest.util.FileUtil;

/**
 * Implementación de AlbumManager
 * @author <a href="mailto:oscarrbr@ext.inditex.com">Óscar Rodríguez Brea</a>
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
	public List<AlbumDTO> getAlbumesUsuario(Long idUsuario) {
		
		ExampleMatcher publicMatcher = ExampleMatcher.matchingAll()
			      .withMatcher("idUsuarioAlta", ExampleMatcher.GenericPropertyMatchers.exact());	
		
		User user = new User();
		user.setId(idUsuario);
		
		Album a = new Album();
		a.setUsuarioAlta(user);
		
		Example<Album> example = Example.of(a,publicMatcher);
		
		List<Album> albumes = albumRepository.findAll(example);
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
	public Boolean deleteAlbum(Long id,Long idUsuario) {
		String folderBackupAlbum = backupConfiguration.getAlbum();
		
		Boolean exito  = Boolean.FALSE;
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
			
			try {
				FileUtils.deleteDirectory(new File(folderBackupAlbumDetail));
				albumRepository.delete(opt.get());
				exito = Boolean.TRUE;	
			}catch(IOException e) {
				exito = Boolean.FALSE;
				e.printStackTrace();
			}
			
		}		
		return exito;
		
	}

}
