package com.oscar.videoteca.rest.util;

import java.io.File;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.oscar.videoteca.rest.config.BackupConfiguration;

/**
 * Utilidades relacionadas con el tratamiento de un vídeo
 * @author <a href="mailto:oscar.rodriguezbrea@gmail.com">Óscar Rodríguez</a>
 *
 */
@Component
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
	 * @param idVideoteca Id de la videoteca 
	 * @return String
	 */
	public String getBackupVideoFolder(Long idVideoteca) {
		StringBuilder path = new StringBuilder();
		path.append(backupConfiguration.getVideo());
		path.append(File.separatorChar);
		path.append(idVideoteca);
		
		return path.toString();	
	}
		
	
	/**
	 * Devuelve la ruta en disco en el que será almacenado un determinado video
	 * @param idVideoteca Long
	 * @param fileName Nombre del fichero 
	 * @return String
	 */
	public String getBackupVideo(Long idVideoteca,String fileName) {
		StringBuilder path = new StringBuilder();
		path.append(getBackupVideoFolder(idVideoteca));
		path.append(File.separatorChar);
		path.append(String.valueOf(idVideoteca).concat("_").concat("_").concat(StringUtils.normalizeSpace(fileName)));
		return path.toString();	
	}
	
	
	/**
	 * Devuelve la ruta relativa de una video en disco para poder visualizar el vídeo
	 * @param fileName Nombre de la fotografía
	 * @param idVideoteca Id de la videoteca 
	 * @param idUsuario Id del usuario
	 * @return String
	 */
	public String getRelativePathVideo(String fileName,Long idVideoteca) {
		StringBuilder path = new StringBuilder();
		path.append(File.separatorChar);
		path.append(idVideoteca);
		path.append(File.separatorChar);
		path.append(fileName);
		
		return path.toString();	
	}
	
	/**
	 * Devuelve el nombre con el que se almacenará un vídeo en disco.
	 * Tendrá el formato [idVideoteca]_[fileName]
	 * @param idVideoteca Id de la videoteca
 	 * @param fileName Nombre del fichero
	 * @return String
	 */
	public String getNameVideoInBackup(Long idVideoteca,String fileName) {
		return String.valueOf(idVideoteca).concat("_").concat(fileName);
	}
}