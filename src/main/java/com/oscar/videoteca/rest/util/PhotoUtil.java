package com.oscar.videoteca.rest.util;

import java.io.File;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.oscar.videoteca.rest.config.BackupConfiguration;

/**
 * Utilidades relacionadas con el tratamiento de una fotografía
 * @author <a href="mailto:oscar.rodriguezbrea@gmail.com">Óscar Rodríguez</a>
 *
 */
@Component
public class PhotoUtil {
	
	@Autowired
	private BackupConfiguration backupConfiguration;
	
	/**
	 * Devuelve un String con la ruta de backup de un álbum de un usuario 
	 * @param idUsuario Long
	 * @return String
	 */
	public String getBackupUserDirectory(Long idUsuario) {
		StringBuilder path = new StringBuilder();
		path.append(backupConfiguration.getAlbum());
		path.append(File.separatorChar);
		path.append(idUsuario);
		
		return path.toString();
	}

	
	/**
	 * Devuelve un String con la ruta de backup de un álbum de un usuario 
	 * @param idAlbum Long
	 * @param idUsuario Long
	 * @return String
	 */
	public String getBackupAlbumDirectory(Long idAlbum,Long idUsuario) {
		StringBuilder path = new StringBuilder();
		path.append(backupConfiguration.getAlbum());
		path.append(File.separatorChar);
		path.append(idUsuario);
		
		path.append(File.separatorChar);
		path.append(idAlbum);
		
		return path.toString();
	}
	
	
	/**
	 * Devuelve la ruta de backup de una fotografía de un álbum en disco
	 * @param idAlbum Id del álbum
	 * @param idUsuario Id del usuario
	 * @param fileName Nombre de la fotografía
	 * @return String
	 */
	public String getBackupPhoto(Long idAlbum,Long idUsuario,String fileName) {
		StringBuilder path = new StringBuilder();
		path.append(getBackupAlbumDirectory(idAlbum, idUsuario));
		path.append(File.separatorChar);
		path.append(fileName);

		return path.toString();	
	}
	
}
