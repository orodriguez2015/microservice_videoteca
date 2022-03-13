package com.oscar.videoteca.rest.manager;

import java.io.File;
import java.io.IOException;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import com.oscar.videoteca.rest.config.BackupConfiguration;
import com.oscar.videoteca.rest.exception.SaveFileException;
import com.oscar.videoteca.rest.model.entity.Album;
import com.oscar.videoteca.rest.model.entity.Foto;
import com.oscar.videoteca.rest.model.entity.User;
import com.oscar.videoteca.rest.model.repository.FotoRepository;
import com.oscar.videoteca.rest.util.FileUtil;

/**
 * Clase FotoManagerImpl
 * @author <a href="mailto:oscar.rodriguezbrea@gmail.com">Óscar Rodríguez Brea</a>
 *
 */
@Service
public class FotoManagerImpl implements FotoManager {

	@Autowired
	private FotoRepository fotoRepository;
	
	@Autowired
	private BackupConfiguration backupConfiguration;
	

	@Override
	public void saveFoto(MultipartFile foto, Long idAlbum, Long idUsuario) throws IOException,SaveFileException {
		
		Boolean userPathCreated  = Boolean.FALSE;
		Boolean albumPathCreated  = Boolean.TRUE;
		
		StringBuilder path = new StringBuilder();
		path.append(backupConfiguration.getAlbum());
		path.append(File.separatorChar);
		path.append(idUsuario);
		
		userPathCreated = FileUtil.createFolder(path.toString());
		
		path.append(File.separatorChar);
		path.append(idAlbum);
		
		albumPathCreated = FileUtil.createFolder(path.toString());
				
		if(Boolean.TRUE.equals(albumPathCreated) && Boolean.TRUE.equals(userPathCreated)) {
			// Si se ha creado el directorio o ya existe, se persiste la foto			
			path.append(File.separatorChar);
			path.append(foto.getOriginalFilename());
			
			
			Boolean continuar = Boolean.FALSE;
			try {
				FileUtil.saveFile(foto.getInputStream(),path.toString());
					
				continuar =Boolean.TRUE;
			}catch(SaveFileException e) {
				throw e;
			}
			
			if(Boolean.TRUE.equals(continuar)) {
				// Si se ha almacenado el fichero en disco => Se almacena registro en BBDD
				User user = new User();
				user.setId(idUsuario);
				
				Album album = new Album();
				album.setId(idAlbum);
				
				Foto f = new Foto();
				f.setNombre(foto.getOriginalFilename());
				f.setTipoMime(foto.getContentType());
				f.setRuta(path.toString());
				f.setRutaMiniatura(path.toString());
				f.setPublico(Boolean.TRUE);
			
				
				f.setUsuario(user);
				f.setAlbum(album);
				fotoRepository.save(f);	
			}
		}
		
	}

}
