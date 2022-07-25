package com.oscar.videoteca.rest.util;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Arrays;
import java.util.Iterator;

import javax.imageio.ImageIO;
import javax.imageio.ImageReader;
import javax.imageio.stream.ImageInputStream;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.oscar.videoteca.rest.config.BackupConfiguration;
import com.oscar.videoteca.rest.exception.SaveFileException;

import lombok.Getter;
import lombok.Setter;

/**
 * Clase FileUtil con operaciones de utilidad para el manejo/trabajo con fichero
 * @author <a href="mailto:oscar.rodriguezbrea@gmail.com">Óscar Rodríguez Brea</a>
 *
 */
@Component
public class FileUtil {
	
	@Autowired
	private BackupConfiguration backupConfiguration;
	
	private final int BUFFER_SIZE = 1024*1024;

	/**
	 * Da de alta una carpeta/directorio en disco siempre y cuando no exista
	 * @param folder Ruta de la carpeta/directorio en disco
	 */
	public Boolean createFolder(String folder) {
		Boolean exito = Boolean.FALSE;
		try {
			File f = new File(folder);
			if(Boolean.FALSE.equals(f.exists())) {
				f.mkdir();
			}
			
			exito =Boolean.TRUE;
		}catch(Exception e) {
			exito =Boolean.FALSE;
		}
		
		return exito;
	}
	
	

	
	
	/**
	 * Permite almacenar un ficheor subido al servidor en una determinada ruta
	 * @param inputStream InputStream
	 * @param filePath Ruta del fichero en disco
	 * @return Boolean
	 * @throws SaveFileException si ocurre algún error
	 */
	public Boolean saveFile(InputStream inputStream,String filePath) throws SaveFileException {
		Boolean exito = Boolean.FALSE;
		
		try {
						
			File f = new File(filePath);
			f.createNewFile();
			
		    OutputStream ficheroSalida = new FileOutputStream(new File(filePath));
            byte[] buf = new byte[BUFFER_SIZE];
            int cantidadLeida;
            while ((cantidadLeida = inputStream.read(buf, 0,BUFFER_SIZE)) > 0){
                ficheroSalida.write(buf, 0, cantidadLeida);
            }
            inputStream.close();
            ficheroSalida.close();
		
			exito =Boolean.TRUE;
			
		}catch(Exception e) {
			exito =Boolean.FALSE;
			throw new SaveFileException("Error al grabar fichero en disco",e);
		}finally {
			close(inputStream);
		}
		return exito;		
	}
	
	
	/**
	 * Borra un fichero de disco
	 * @param f File
	 * @return True si se ha borrado y false en caso contrario
	 */
	public Boolean deleteFile(File f) {
		Boolean exito = Boolean.FALSE;
		if(f!=null) {
			exito = f.delete();
		}
		
		return exito;
	}
	
	
	/**
	 * Borra un directorio y todo su contenido
	 * @param f File que representa un directorio
	 * @return True si se ha eliminado y false en caso contrario
	 */
	public Boolean deleteDirectory(File f) {
		Boolean exito  =Boolean.FALSE;
		if(f!=null && f.exists() && Boolean.TRUE.equals(f.isDirectory())) {
			File[] files = f.listFiles();
			
			if(files!=null && files.length>0) {
				
				Arrays.asList(files).stream().forEach(file ->{
					
					if(Boolean.TRUE.equals(file.isFile())) {
						this.deleteFile(file);
					} else {
						deleteDirectory(file);
					}
					
				});
			} else  {
				f.delete();
				exito = Boolean.TRUE;
			}
			
		}
		
		return exito;
	}
	

	/**
	 * Cierra un InputStream
	 * @param is InputStream
	 */
	public void close(InputStream is) {
		try {
			if(is!=null) {
				is.close();
			}
		}catch(IOException e) {
			
		}
	}
	
	
	/**
	 * Cierra un FileOutputStream
	 * @param fos FileOutputStream
	 */
	public void close(FileOutputStream fos) {
		try {
			if(fos!=null) {
				fos.close();
			}
		}catch(IOException e) {
			
		}		
	}


	/**
	 * Devuelve un objeto instancia de PhotoSize con el tamaño
	 * @param f File
	 * @return FileUtil.PhotoSize
	 * @throws IOException
	 */
	
	public PhotoSize getPhotoSize(File f) throws IOException {
		PhotoSize ps = new PhotoSize();
			
		Iterator<ImageReader> readers = ImageIO.getImageReadersByFormatName("jpeg");
		ImageReader reader = (ImageReader)readers.next();
		ImageInputStream iis = ImageIO.createImageInputStream(f);
		reader.setInput(iis, true);

		ps.setHeight(reader.getHeight(0));
		ps.setWidth(reader.getWidth(0));
		
		return ps;
	}

	

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
	
	
	/**
	 * Devuelve la ruta relativa de una fotografía en disco y que se utilizará para poder
	 * mostrar la fotografía
	 * @param fileName Nombre de la fotografía
	 * @param idAlbum Id del álbum
	 * @param idUsuario Id del usuario
	 * @return String
	 */
	public String getRelativePathPhoto(String fileName,Long idAlbum,Long idUsuario) {
		StringBuilder path = new StringBuilder();
		path.append(File.separatorChar);
		path.append(idUsuario);
		path.append(File.separatorChar);
		path.append(idAlbum);
		path.append(File.separatorChar);
		path.append(fileName);
		
		return path.toString();	
	}
	
	
	/**
	 * Devuelve la ruta completa en la que se almacena en disco los vídeos de una determinada videoteca
	 * @param idUsuario Id del usuario
	 * @param idVideoteca Id de la videoteca 
	 * @return Strig
	 */
	public String getBackupVideoFolder(Long idUsuario,Long idVideoteca) {
		StringBuilder path = new StringBuilder();
		path.append(backupConfiguration.getVideo());
		path.append(File.separatorChar);
		path.append(idUsuario);
		path.append(File.separatorChar);
		path.append(idUsuario);
		path.append(File.separatorChar);
		path.append(idVideoteca);
		
		return path.toString();	
	}
		
	
	
	/**
	 * Clase PhotoSize
	 * @author <a href="mailto:oscar.rodriguezbrea@gmail.com">Óscar Rodríguez Brea</a>
	 *
	 */
	@Getter
	@Setter
	public static class PhotoSize {
		private Integer height;
		private Integer width;
		
	}
	
	
}