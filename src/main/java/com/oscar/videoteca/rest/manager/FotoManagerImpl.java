package com.oscar.videoteca.rest.manager;

import java.io.File;
import java.io.IOException;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import com.oscar.videoteca.rest.exception.SaveFileException;
import com.oscar.videoteca.rest.exception.SavePhotoException;
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
	private FileUtil fileUtil;

	@Override
	public void saveFoto(MultipartFile foto, Long idAlbum, Long idUsuario) throws IOException,SaveFileException, SavePhotoException {
		
		Boolean userPathCreated  = Boolean.FALSE;
		Boolean albumPathCreated  = Boolean.TRUE;		

		userPathCreated = fileUtil.createFolder(fileUtil.getBackupUserDirectory(idUsuario));		
		albumPathCreated = fileUtil.createFolder(fileUtil.getBackupAlbumDirectory(idAlbum, idUsuario));
				
		if(Boolean.TRUE.equals(albumPathCreated) && Boolean.TRUE.equals(userPathCreated)) {
			// Si se ha creado el directorio o ya existe, se persiste la foto			
			
			String path = fileUtil.getBackupPhoto(idAlbum, idUsuario,foto.getOriginalFilename());
			
			Boolean continuar = Boolean.FALSE;
			try {
				fileUtil.saveFile(foto.getInputStream(),path);
					
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
				f.setRuta(path);
				f.setPublico(Boolean.TRUE);
		
				File fotografia = new File(path);
				
		
				FileUtil.PhotoSize size = fileUtil.getPhotoSize(fotografia);
				f.setAlto(size.getHeight());
				f.setAncho(size.getWidth());
				
				f.setNumeroVisualizaciones(Integer.valueOf(0));			
				f.setUsuario(user);
				f.setAlbum(album);
				try {
					fotoRepository.saveAndFlush(f);
				}catch(Exception e) {
					e.printStackTrace();
					
					// Si ha ocurrido alǵun error, entonces se borra la foto del disco
					fileUtil.deleteFile(fotografia);
					
					throw new SavePhotoException(e.getMessage());
				}
			}
		}
		
	}

}
