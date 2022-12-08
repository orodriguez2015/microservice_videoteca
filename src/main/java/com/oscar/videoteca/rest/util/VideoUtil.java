package com.oscar.videoteca.rest.util;

import java.io.File;

import org.springframework.beans.factory.annotation.Autowired;

import com.oscar.videoteca.rest.config.BackupConfiguration;

/**
 * Utilidades relacionadas con el tratamiento de un vídeo
 * @author <a href="mailto:oscar.rodriguezbrea@gmail.com">Óscar Rodríguez</a>
 *
 */
public class VideoUtil {

	@Autowired
	private BackupConfiguration backupConfiguration;
	
	/**
	 * Constructor
	 */
	private VideoUtil() {
		
	}
	
	/**
	 * Devuelve la ruta completa en la que se almacena en disco los vídeos de una determinada videoteca
	 * @param idUsuario Id del usuario
	 * @param idVideoteca Id de la videoteca 
	 * @return String
	 */
	public String getBackupVideoFolder(Long idUsuario,Long idVideoteca) {
		StringBuilder path = new StringBuilder();
		path.append(backupConfiguration.getVideo());
		path.append(File.separatorChar);
		path.append(idUsuario);
		path.append(File.separatorChar);
		path.append(idVideoteca);
		
		return path.toString();	
	}
	
	
	/**
	 * Devuelve la ruta en disco en el que será almacenado un determinado video
	 * @param idVideoteca Id de la videoteca
	 * @param idUsuario Id del usuario
	 * @param fileName Nombre del fichero
	 * @return String
	 */
	public String getBackupVideo(Long idVideoteca,Long idUsuario,String fileName) {
		StringBuilder path = new StringBuilder();
		path.append(getBackupVideoFolder(idUsuario,idVideoteca));
		path.append(File.separatorChar);
		path.append(getNameVideoInBackup(idVideoteca, fileName));
		return path.toString();	
	}
	
	/**
	 * Devuelve el nombre con el que se almacenará un vídeo en disco.
	 * Tendrá el formato [idVideoteca]_[fileName]
	 * @param idVideoteca Id de la videoteca
 	 * @param fileName Nombre del fichero
	 * @return String
	 */
	public static String getNameVideoInBackup(Long idVideoteca,String fileName) {
		return String.valueOf(idVideoteca).concat("_").concat(fileName);
	}
}