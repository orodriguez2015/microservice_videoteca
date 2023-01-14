package com.oscar.videoteca.rest.manager.impl;

import java.io.File;
import java.io.IOException;
import java.util.Calendar;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Example;
import org.springframework.data.domain.ExampleMatcher;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import com.oscar.videoteca.rest.exception.ErrorPublishVideoException;
import com.oscar.videoteca.rest.exception.SaveFileException;
import com.oscar.videoteca.rest.exception.SaveVideoException;
import com.oscar.videoteca.rest.exception.VideoNotFoundException;
import com.oscar.videoteca.rest.manager.VideoManager;
import com.oscar.videoteca.rest.model.entity.User;
import com.oscar.videoteca.rest.model.entity.Video;
import com.oscar.videoteca.rest.model.entity.Videoteca;
import com.oscar.videoteca.rest.model.repository.VideoRepository;
import com.oscar.videoteca.rest.util.FileUtil;
import com.oscar.videoteca.rest.util.VideoUtil;

@Service
public class VideoManagerImpl implements VideoManager{
	
	@Autowired
	private FileUtil fileUtil;

	@Autowired
	private VideoUtil videoUtil;
	
	@Autowired
	private VideoRepository videoRepository;

	@Override
	public void saveVideo(MultipartFile file, Long idVideoteca, Long idUsuario) throws SaveVideoException {	
		// Si no existe se crea el directorio del usuario dentro del directorio de backup de álbum			
		fileUtil.createFolder(videoUtil.getBackupUserDirectory(idUsuario));
		// Dentro del directorio de backup de los álbumes del usuario, se crea la carpeta de la videoteca (idVideoteca)
		fileUtil.createFolder(videoUtil.getBackupVideoFolder(idVideoteca, idUsuario));
		// Ruta en disco del fichero en el backup. El nombe del fichero se modifica por la combinación del día y hora 
		String path= videoUtil.getFilePathInBackup(idVideoteca,idUsuario,videoUtil.getNewFileNameVideo(file.getOriginalFilename()));
		String relativePath = videoUtil.getFileRelativePathInBackup(idVideoteca, idUsuario, videoUtil.getNewFileNameVideo(file.getOriginalFilename()));
		
		User user = User.builder().id(idUsuario).build();
		Videoteca videoteca = Videoteca.builder().id(idVideoteca).build();
		
		Video video = Video.builder().
		fechaAlta(Calendar.getInstance().getTime()).
		nombre(file.getOriginalFilename()).
		publico(Boolean.TRUE).
		usuario(user).
		ruta(path).
		rutaRelativa(relativePath).
		videoteca(videoteca).build();
			
		try {
			// Persiste el vídeo en BBDD
			video = videoRepository.saveAndFlush(video);
						
			// Persiste el vídeo en disco		
			this.saveFileToDisk(file, path);
			
		
		}catch(Exception e) {
			// Si ha ocurrido alǵun error, entonces se borra la foto del disco
			fileUtil.deleteFile(new File(path));
			throw new SaveVideoException(e.getMessage());
		}
		
	}
	
	
	/**
	 * Persiste un video en disco
	 * @param file Fichero subido al servicor
	 * @param path Ruta del fichero en la carpeta de backup
	 * @throws SaveVideoException si ocurre un error al persistir el vídeo
	 */
	private void saveFileToDisk(MultipartFile file,String path) throws SaveVideoException{
		try {
			fileUtil.saveFile(file.getInputStream(),path);		
			
		}catch(SaveFileException | IOException e) {
			throw new SaveVideoException("Se ha producido un error al persistir el vídeo en disco",e);
		}
				
	}


	@Override
	public Boolean publishVideo(Long idVideo, Long idUsuario, Long value) throws VideoNotFoundException {
		Boolean salida =Boolean.FALSE;
		
		try {
			// Se recupera el vídeo de la BBDD
			Video video = this.getVideo(idVideo, idUsuario);
			video.setPublico(Boolean.FALSE);
			
			if(value.equals(1L)) {
				video.setPublico(Boolean.TRUE);	
			}
			
			this.videoRepository.saveAndFlush(video);
			salida = Boolean.TRUE;
		}catch(Exception e) {
			salida =Boolean.FALSE;
			throw new ErrorPublishVideoException("Error al publicar/despublicar un vídeo");
		}
		
		return salida;	
	}
	
	/**
	 * Recupera un determinado vídeo de un usuario
	 * @param idVideo Id del vídeo
	 * @param idUsuario Id del usuairo
	 * @return Video
	 * @throws VideoNotFoundException sino existe el vídeo
	 * 
	 */
	public Video getVideo(Long idVideo,Long idUsuario) throws VideoNotFoundException{
		ExampleMatcher exampleMatcher = ExampleMatcher.matchingAll()
			      .withMatcher("id", ExampleMatcher.GenericPropertyMatchers.exact().ignoreCase())
			      .withMatcher("idUsuario", ExampleMatcher.GenericPropertyMatchers.exact().ignoreCase());
		
		Video salida = null;
		User user = User.builder().id(idUsuario).build();
		Video video = Video.builder().id(idVideo).usuario(user).build();
	
		Example<Video> example = Example.of(video,exampleMatcher);
		Optional<Video> opt = this.videoRepository.findOne(example);
		if(Boolean.FALSE.equals(opt.isPresent())) {
			throw new VideoNotFoundException("No existe el vídeo"); 
		} else {
			salida = opt.get();
		}
		
		return salida;
	}



}
