package com.oscar.videoteca.rest.manager.impl;

import java.util.List;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Example;
import org.springframework.data.domain.ExampleMatcher;
import org.springframework.stereotype.Service;

import com.oscar.videoteca.rest.config.BackupConfiguration;
import com.oscar.videoteca.rest.dto.CreateVideotecaDTO;
import com.oscar.videoteca.rest.dto.VideotecaDTO;
import com.oscar.videoteca.rest.dto.mapping.VideotecaConverter;
import com.oscar.videoteca.rest.exception.VideotecasNotFoundException;
import com.oscar.videoteca.rest.manager.VideotecaManager;
import com.oscar.videoteca.rest.model.entity.User;
import com.oscar.videoteca.rest.model.entity.Videoteca;
import com.oscar.videoteca.rest.model.repository.VideotecaRepository;
import com.oscar.videoteca.rest.util.FileUtil;

import ch.qos.logback.core.pattern.Converter;

/**
 * Clase VideotecaManagerImpl
 * @author <a href="mailto:oscar.rodriguezbrea@gmail.com">Óscar Rodríguez Brea</a>
 *
 */
@Service
public class VideotecaManagerImpl implements VideotecaManager {

	@Autowired
	private VideotecaRepository videotecaRepository;
	
	@Autowired
	private VideotecaConverter videotecaConverter;
	
	@Autowired
	private BackupConfiguration backupConfiguration;
	
	@Autowired
	private FileUtil fileUtil;
	
	
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

	@Override
	public Boolean checkVideoteca(Long idUsuario, String folder) {
		
		ExampleMatcher publicMatcher =  ExampleMatcher.matchingAll().withMatcher("idUsuario",ExampleMatcher.GenericPropertyMatchers.exact()).
		withMatcher("rutaCarpetaRelativa",ExampleMatcher.GenericPropertyMatchers.exact());
		
		User user = new User();
		user.setId(idUsuario);
		
		Videoteca v = new Videoteca();
		v.setUsuario(user);
		
		Example<Videoteca> example = Example.of(v,publicMatcher);
		List<Videoteca> list = videotecaRepository.findAll(example);
		
		if(Boolean.TRUE.equals(list.isEmpty())) {
			return Boolean.FALSE;
		}
		
		return Boolean.TRUE;
	}

	@Override
	public VideotecaDTO save(CreateVideotecaDTO create) {
		String folderBackupVideo = backupConfiguration.getVideo();
		if(StringUtils.isNotEmpty(folderBackupVideo)) {
			// Se crea la carpeta raíz de backup de videoteca sino existe
			fileUtil.createFolder(folderBackupVideo);	
		} 	
		
		Videoteca videoteca = videotecaConverter.convertTo(create);
		
		videoteca = videotecaRepository.saveAndFlush(videoteca);
		return videotecaConverter.convertTo(videoteca);		
	}

}
