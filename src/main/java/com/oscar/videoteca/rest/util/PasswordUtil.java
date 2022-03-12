package com.oscar.videoteca.rest.util;

import org.apache.commons.codec.digest.DigestUtils;
import org.apache.commons.lang3.StringUtils;

/**
 * Clase PasswordUtil
 * @author <a href="mailto:oscar.rodriguezbrea@gmail.com">Óscar Rodríguez Brea</a>
 *
 */
public class PasswordUtil {

	/**
	 * Método estático que devuelve el hash SHA1 de una cadena de caracteres
	 * @param password String
	 * @return String
	 */
	public static String getSha1(String password) {
		return DigestUtils.sha1Hex(password);
	}
	
	
	/**
	 * Método que devuelve el hash MD5 correspondiente a un String
	 * @param dato String
	 * @return String
	 */
	public static String getMd5(String dato) {
		return DigestUtils.md5Hex(dato);
	}
	
	/**
	 * Comprueba si el hash de parámetro <param>hash</param> coincide con el hash SHA1 que se obtiene a partir del 
	 * @param hash String que contiene un hash SHA1
	 * @param data String del cual se quiere obtener el hash1 y compararlo con el del parámetro <param>hash</param>
	 * @return True si hay coincidencia y false en caso contrario
	 */
	public static Boolean matchHashSha1(String hash,String data) {
		Boolean exito = Boolean.FALSE;
		
		if(StringUtils.isNotEmpty(hash) && StringUtils.isNotEmpty(data) && getSha1(data).equals(hash)) {
			exito = Boolean.TRUE;
		}
		
		return exito;
	}
	
}
