package com.oscar.videoteca.rest.util;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import com.oscar.videoteca.rest.exception.SaveFileException;

import lombok.Getter;
import lombok.Setter;

/**
 * Clase FileUtil con operaciones de utilidad para el manejo/trabajo con fichero
 * @author <a href="mailto:oscar.rodriguezbrea@gmail.com">Óscar Rodríguez Brea</a>
 *
 */
public class FileUtil {
	
	private static final int BUFFER_SIZE = 1024*1024;

	/**
	 * Da de alta una carpeta/directorio en disco siempre y cuando no exista
	 * @param folder Ruta de la carpeta/directorio en disco
	 */
	public static Boolean createFolder(String folder) {
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
	public static Boolean saveFile(InputStream inputStream,String filePath) throws SaveFileException {
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
	 * Cierra un InputStream
	 * @param is InputStream
	 */
	public static void close(InputStream is) {
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
	public static void close(FileOutputStream fos) {
		try {
			if(fos!=null) {
				fos.close();
			}
		}catch(IOException e) {
			
		}		
	}


	
	public static PhotoSize getPhotoSize(File f) {
		PhotoSize ps = new PhotoSize();
		
		return ps;
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