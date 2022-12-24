package com.oscar.videoteca.rest.manager.impl;

import java.io.File;
import java.io.IOException;
import java.util.Calendar;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import com.oscar.videoteca.rest.exception.SaveFileException;
import com.oscar.videoteca.rest.exception.SaveVideoException;
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
		Boolean userPathCreated = fileUtil.createFolder(videoUtil.getBackupUserDirectory(idUsuario));
		// Dentro del directorio de backup de los álbumes del usuario, se crea la carpeta de la videoteca (idVideoteca)
		Boolean videotecaPathCreated = fileUtil.createFolder(videoUtil.getBackupVideoFolder(idVideoteca, idUsuario));
		String path = videoUtil.getBackupVideo(idVideoteca,idUsuario,file.getOriginalFilename());
		
		User user = User.builder().id(idUsuario).build();
		Videoteca videoteca = Videoteca.builder().id(idVideoteca).build();
		
		Video video = Video.builder().fechaAlta(Calendar.getInstance().getTime()).
		nombre(videoUtil.getNameVideoInBackup(idVideoteca, file.getOriginalFilename())).
		publico(Boolean.TRUE).
		usuario(user).
		ruta(videoUtil.getRelativePathVideo(file.getOriginalFilename(), idVideoteca)).
		videoteca(videoteca).build();
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		try {
			// Persiste el vídeo en BBDD
			videoRepository.saveAndFlush(video);
			// Persiste el vídeo en disco
			this.saveFileToDisk(file, path);
			
		}catch(Exception e) {
			e.printStackTrace();
			
			// Si ha ocurrido alǵun error, entonces se borra la foto del disco
			fileUtil.deleteFile(new File(path));
			
			throw new SaveVideoException(e.getMessage());
		}
		
	}
	
	
	private void saveFileToDisk(MultipartFile file,String path) throws SaveVideoException{
		try {
			fileUtil.saveFile(file.getInputStream(),path);		
			
		}catch(SaveFileException | IOException e) {
			throw new SaveVideoException("Se ha producido un error al persistir el vídeo en disco",e);
		}
				
	}

}
