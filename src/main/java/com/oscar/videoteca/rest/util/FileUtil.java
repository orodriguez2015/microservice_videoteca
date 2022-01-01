package com.oscar.videoteca.rest.util;

import java.io.File;

/**
 * Clase FileUtil con operaciones de utilidad para el manejo/trabajo con fichero
 * @author <a href="mailto:oscarrbr@ext.inditex.com">Óscar Rodríguez Brea</a>
 *
 */
public class FileUtil {

	/**
	 * Da de alta una carpeta/directorio en disco
	 * @param folder String que contiene 
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
	
	
}