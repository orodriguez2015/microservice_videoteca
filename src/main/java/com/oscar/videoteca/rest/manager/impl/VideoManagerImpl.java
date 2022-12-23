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
	public void saveVideo(MultipartFile foto, Long idVideoteca, Long idUsuario) throws SaveVideoException {	
	
		// Si se ha creado el directorio o ya existe, se persiste el vídeo			
		
		String path = videoUtil.getBackupVideo(idVideoteca,foto.getOriginalFilename());
		
		Boolean continuar = Boolean.FALSE;
		try {
			fileUtil.saveFile(foto.getInputStream(),path);		
			continuar =Boolean.TRUE;
			
		}catch(SaveFileException | IOException e) {
			throw new SaveVideoException("Se ha producido un error al persistir el vídeo en disco",e);
		}
				
		if(Boolean.TRUE.equals(continuar)) {
			// Si se ha almacenado el fichero en disco => Se almacena registro en BBDD
			User user = new User();
			user.setId(idUsuario);
							
			Videoteca videoteca = new Videoteca();
			videoteca.setId(idVideoteca);
			
			Video video = new Video();
			video.setFechaAlta(Calendar.getInstance().getTime());
			video.setNombre(videoUtil.getNameVideoInBackup(idVideoteca, foto.getOriginalFilename()));
			video.setPublico(Boolean.TRUE);
			video.setUsuario(user);
			video.setRuta(videoUtil.getRelativePathVideo(foto.getOriginalFilename(), idVideoteca));
			video.setVideoteca(videoteca);
			
			File file = new File(path);
		
			try {
				videoRepository.saveAndFlush(video);
			}catch(Exception e) {
				e.printStackTrace();
				
				// Si ha ocurrido alǵun error, entonces se borra la foto del disco
				fileUtil.deleteFile(file);
				
				throw new SaveVideoException(e.getMessage());
			}
		}	
	}

}
