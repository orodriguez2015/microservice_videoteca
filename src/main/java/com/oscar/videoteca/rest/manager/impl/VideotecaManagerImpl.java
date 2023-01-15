package com.oscar.videoteca.rest.manager.impl;

import java.util.List;
import java.util.Optional;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Example;
import org.springframework.data.domain.ExampleMatcher;
import org.springframework.stereotype.Service;

import com.oscar.videoteca.rest.config.BackupConfiguration;
import com.oscar.videoteca.rest.dto.CreateVideotecaDTO;
import com.oscar.videoteca.rest.dto.VideotecaDTO;
import com.oscar.videoteca.rest.dto.mapping.VideotecaConverter;
import com.oscar.videoteca.rest.exception.ErrorDeleteVideotecaException;
import com.oscar.videoteca.rest.exception.VideotecaNotFoundException;
import com.oscar.videoteca.rest.exception.VideotecasNotFoundException;
import com.oscar.videoteca.rest.manager.VideotecaManager;
import com.oscar.videoteca.rest.model.entity.User;
import com.oscar.videoteca.rest.model.entity.Video;
import com.oscar.videoteca.rest.model.entity.Videoteca;
import com.oscar.videoteca.rest.model.repository.VideoRepository;
import com.oscar.videoteca.rest.model.repository.VideotecaRepository;
import com.oscar.videoteca.rest.util.FileUtil;
import com.oscar.videoteca.rest.util.ResourceVisibilityEnum;

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
	private VideoRepository videoRepository;
	
	
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
			
		Example<Videoteca> example = Example.of(Videoteca.builder().publico(Boolean.TRUE).build(), publicMatcher);
		return videotecaConverter.convertTo(videotecaRepository.findAll(example));
	}

	@Override
	public List<VideotecaDTO> getVideotecasUsuario(Long id) throws VideotecasNotFoundException {
		
		ExampleMatcher publicMatcher = ExampleMatcher.matchingAll()
			      .withMatcher("idUsuario", ExampleMatcher.GenericPropertyMatchers.exact());	      
			
		User user = User.builder().id(id).build();
		Videoteca v = Videoteca.builder().usuario(user).build();
		
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
		
		User user = User.builder().id(idUsuario).build();
		Videoteca v = Videoteca.builder().usuario(user).build();
		
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

	@Override
	public VideotecaDTO getVideoteca(Long id,Long idUsuario,ResourceVisibilityEnum visibility) throws VideotecaNotFoundException {
		ExampleMatcher publicMatcher = ExampleMatcher.matchingAll()
			      .withMatcher("id", ExampleMatcher.GenericPropertyMatchers.exact())
			      .withMatcher("idUsuario", ExampleMatcher.GenericPropertyMatchers.exact());	  
			
		Videoteca v = Videoteca.builder().id(id).build();
		
		Example<Videoteca> example = Example.of(v, publicMatcher);		
		
		Optional<Videoteca> opt = videotecaRepository.findOne(example);
		if(Boolean.TRUE.equals(opt.isEmpty())) {
			throw new VideotecaNotFoundException("No existe la videoteca");
		} else 	
		if(!visibility.equals(ResourceVisibilityEnum.NONE)) {
			// Si se recuperan los vídeos
		}
		
		return videotecaConverter.convertTo(opt.get());
	}

	@Override
	public VideotecaDTO update(CreateVideotecaDTO update) throws VideotecaNotFoundException {
		Optional<Videoteca> opt = videotecaRepository.findById(update.getId());
		VideotecaDTO nuevo = null;
		if(Boolean.FALSE.equals(opt.isPresent())) {
			throw new VideotecaNotFoundException("No existe la videoteca que se pretende editar");
		} else {
			Videoteca original = opt.get();
			original.setNombre(update.getNombre());
			original.setPublico(update.getPublico());
			nuevo = videotecaConverter.convertTo(videotecaRepository.saveAndFlush(original));
			
		}
		return nuevo;
	}

	
	@Override
	public Boolean delete(Long id,Long idUsuario) throws ErrorDeleteVideotecaException {
		Boolean exito  = Boolean.FALSE;
		try {
			ExampleMatcher exampleMatcher = ExampleMatcher.matchingAll()
				      .withMatcher("id", ExampleMatcher.GenericPropertyMatchers.exact().ignoreCase())
				      .withMatcher("idUsuario", ExampleMatcher.GenericPropertyMatchers.exact().ignoreCase());
	
			User user = User.builder().id(idUsuario).build();	
	
			Videoteca video = Videoteca.builder().id(id).usuario(user).build();
			
			Example<Videoteca> example = Example.of(video,exampleMatcher);		
			Optional<Videoteca> opt = videotecaRepository.findOne(example);
			
			if(opt.isPresent()) {
				
		
				exito = Boolean.TRUE;				
			}
		}catch(Exception e) {
			throw new ErrorDeleteVideotecaException("Error al eliminar la videoteca",e);
		}
		return exito;
		
	}

	@Override
	public Boolean existsById(Long id) {
		return videotecaRepository.existsById(id);
	}

	@Override
	public VideotecaDTO getVideos(Long idVideoteca,ResourceVisibilityEnum visibility) throws VideotecaNotFoundException {
		VideotecaDTO videotecaDTO = null;
		ExampleMatcher publicMatcher = ExampleMatcher.matchingAll()
			      .withMatcher("id", ExampleMatcher.GenericPropertyMatchers.exact())
			      .withMatcher("idUsuario", ExampleMatcher.GenericPropertyMatchers.exact());
			
		Videoteca videoteca = Videoteca.builder().id(idVideoteca).build();
		
		
		Example<Videoteca> example = Example.of(videoteca, publicMatcher);		
		
		Optional<Videoteca> opt = videotecaRepository.findOne(example);
		if(Boolean.TRUE.equals(opt.isEmpty())) {
			throw new VideotecaNotFoundException("No existe la videoteca");
		} else {
			// Se ha recuperado la videoteca, y ahora se recuperan los vídeos de la misma
			publicMatcher = ExampleMatcher.matchingAll().withMatcher("idVideoteca",ExampleMatcher.GenericPropertyMatchers.exact());
			Video.VideoBuilder videoBuilder = Video.builder().videoteca(videoteca);
			
			if(visibility.equals(ResourceVisibilityEnum.PUBLISHED)) {
				// Se recuperan los vídeos públicos si así se ha indicado por paráemtros
				publicMatcher.withMatcher("publico",ExampleMatcher.GenericPropertyMatchers.exact());
				videoBuilder.publico(Boolean.TRUE);				
			}
			
			Video video  = videoBuilder.build(); 
			Example<Video> optv = Example.of(video,publicMatcher);
			List<Video> videos = videoRepository.findAll(optv);
					
			videotecaDTO = this.videotecaConverter.convertTo(opt.get());
			videotecaDTO.setVideos(this.videotecaConverter.convertToListVideoDTO(videos));

		}
		
		return videotecaDTO;
	}	
	
}