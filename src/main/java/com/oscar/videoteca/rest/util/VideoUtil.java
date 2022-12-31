package com.oscar.videoteca.rest.util;

import java.io.File;

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
	
	@Autowired
	private FileUtil fileUtil;
	
	/**
	 * Constructor
	 */
	private VideoUtil() {
		
	}
	

	
	/**
	 * Devuelve un String con la ruta de backup de un álbum de un usuario 
	 * @param idUsuario Long
	 * @return String
	 */
	public String getBackupUserDirectory(Long idUsuario) {
		StringBuilder path = new StringBuilder();
		path.append(backupConfiguration.getVideo());
		path.append(File.separatorChar);
		path.append(idUsuario);
		
		return path.toString();
	}

	
	
	/**
	 * Devuelve la ruta completa en la que se almacena en disco los vídeos de una determinada videoteca
	 * @param idVideoteca Id de la videoteca 
	 * @return String
	 */
	public String getBackupVideoFolder(Long idVideoteca,Long idUsuario) {
		StringBuilder path = new StringBuilder();
		path.append(backupConfiguration.getVideo());
		path.append(File.separatorChar);
		path.append(idUsuario);
		
		path.append(File.separatorChar);
		path.append(idVideoteca);
		
		return path.toString();	
	}
	
	
	/**
	 * Devuelve la ruta relativa en disco en el que será almacenado un determinado video
	 * @param idVideoteca Long
	 * @param idUsuario Long
	 * @param fileName Nombre del fichero 
	 * @return String
	 */
	public String getFileRelativePathInBackup(Long idVideoteca,Long idUsuario,String fileName) {
		StringBuilder path = new StringBuilder();
		path.append(File.separatorChar);
		path.append(idUsuario);
		path.append(File.separatorChar);
		path.append(idVideoteca);
		path.append(File.separatorChar);
		path.append(String.valueOf(idVideoteca).concat("_").concat(fileName));
		return path.toString();	
	}
	
	
	/**
	 * Devuelve la ruta en disco en el que será almacenado un determinado video
	 * @param idVideoteca Long
	 * @param idUsuario Long
	 * @param fileName Nombre del fichero 
	 * @return String
	 */
	public String getFilePathInBackup(Long idVideoteca,Long idUsuario,String fileName) {
		StringBuilder path = new StringBuilder();
		path.append(getBackupVideoFolder(idVideoteca,idUsuario));
		path.append(File.separatorChar);
		path.append(String.valueOf(idVideoteca).concat("_").concat(fileName));
		return path.toString();	
	}
	
	
	/**
	 * A partir del nombre de un fichero genera uno nuevo con nombre de la fecha y hora actual
	 * @param oldFileName Nombre anterior del 
	 * @return
	 */
	public String getNewFileNameVideo(String oldFileName) {
		StringBuilder sb = new StringBuilder();
		sb.append(DateOperations.getTimestamp(DateOperations.DDMMYYYY_hhmmss));
		sb.append(".");
		sb.append(fileUtil.getExtension(oldFileName));
		
		return sb.toString();
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
	
}