package com.oscar.videoteca.rest.util;

import java.io.File;
import java.io.FileOutputStream;
import java.io.InputStream;

import org.springframework.web.multipart.MultipartFile;

/**
 * Clase FileUtil con operaciones de utilidad para el manejo/trabajo con fichero
 * @author <a href="mailto:oscar.rodriguezbrea@gmail.com">Óscar Rodríguez Brea</a>
 *
 */
public class FileUtil {

	/**
	 * Da de alta una carpeta/directorio en disco siempre y cuando no exista
	 * @param folder Ruta de la carpeta/directorio en disco
	 */
	public static Boolean createFolder(String folder) {
		Boolean exito = Boolean.FALSE;
		try {
			File f = new File(folder);
			if(Boolean.TRUE.equals(f.isDirectory()) && Boolean.FALSE.equals(f.exists())) {
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
	 * @param multipartFile MultipartFile
	 * @param path String
	 * @return Boolean
	 */
	public static Boolean saveFile(MultipartFile multipartFile,String path) {

		Boolean exito = Boolean.FALSE;
		try {
			
			String fileName = multipartFile.getName();
			String originalFileName = multipartFile.getOriginalFilename();
			InputStream is = multipartFile.getInputStream();
			
			System.out.println("filename = " + fileName);
			System.out.println("oringal filename = " + originalFileName);
			
			
			File f = new File(path);
			FileOutputStream fos = new FileOutputStream(f);
			if(Boolean.FALSE.equals(f.exists())) {
				f.mkdir();
			}
			
			exito =Boolean.TRUE;
		}catch(Exception e) {
			exito =Boolean.FALSE;
		}
		
		return exito;		
	}
	
}