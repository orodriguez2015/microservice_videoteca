package com.oscar.videoteca.rest.manager.impl;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Example;
import org.springframework.data.domain.ExampleMatcher;
import org.springframework.stereotype.Service;

import com.oscar.videoteca.rest.dto.VideotecaDTO;
import com.oscar.videoteca.rest.dto.mapping.VideotecaConverter;
import com.oscar.videoteca.rest.exception.VideotecasNotFoundException;
import com.oscar.videoteca.rest.manager.VideotecaManager;
import com.oscar.videoteca.rest.model.entity.User;
import com.oscar.videoteca.rest.model.entity.Videoteca;
import com.oscar.videoteca.rest.model.repository.VideotecaRepository;

/**
 * Clase VideotecaManagerImpl
 * @author <a href="mailto:oscarrbr@ext.inditex.com">Óscar Rodríguez Brea</a>
 *
 */
@Service
public class VideotecaManagerImpl implements VideotecaManager {

	@Autowired
	private VideotecaRepository videotecaRepository;
	
	@Autowired
	private VideotecaConverter videotecaConverter;
	
	@Override
	public List<VideotecaDTO> getVideotecasPublicas() {
		
		ExampleMatcher publicMatcher = ExampleMatcher.matchingAll()
			      .withMatcher("publico", ExampleMatcher.GenericPropertyMatchers.exact());	      
		
		Videoteca v = new Videoteca();
		v.setPublico(Boolean.TRUE);
		
		Example<Videoteca> example = Example.of(v, publicMatcher);
		return videotecaConverter.convertTo(videotecaRepository.findAll(example));
	}

	@Override
	public List<VideotecaDTO> getVideotecasUsuario(Long id) throws VideotecasNotFoundException {
		
		ExampleMatcher publicMatcher = ExampleMatcher.matchingAll()
			      .withMatcher("idUsuario", ExampleMatcher.GenericPropertyMatchers.exact());	      
		
		Videoteca v = new Videoteca();
		User user = new User();
		user.setId(id);
		v.setUsuario(user);
		
		Example<Videoteca> example = Example.of(v, publicMatcher);		
		List<Videoteca> videotecas = videotecaRepository.findAll(example);
		if(videotecas==null || Boolean.TRUE.equals(videotecas.isEmpty())) {
			throw new VideotecasNotFoundException("El usuario no tiene videotecas");
		}		
		
		return videotecaConverter.convertTo(videotecas);
	}

}
