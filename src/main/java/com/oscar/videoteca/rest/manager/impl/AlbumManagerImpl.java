package com.oscar.videoteca.rest.manager.impl;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Example;
import org.springframework.data.domain.ExampleMatcher;
import org.springframework.stereotype.Service;

import com.oscar.videoteca.rest.dto.AlbumDTO;
import com.oscar.videoteca.rest.dto.mapping.AlbumConverter;
import com.oscar.videoteca.rest.manager.AlbumManager;
import com.oscar.videoteca.rest.model.entity.Album;
import com.oscar.videoteca.rest.model.repository.AlbumRepository;

/**
 * Clase AlbumManagerImpl
 * @author <a href="mailto:oscarrbr@ext.inditex.com">Óscar Rodríguez Brea</a>
 *
 */
@Service
public class AlbumManagerImpl implements AlbumManager {

	@Autowired
	private AlbumRepository albumRepository;
	
	@Autowired
	private AlbumConverter converter;
	
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

}
