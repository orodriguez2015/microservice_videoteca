package com.oscar.videoteca.rest.exception;

/**
 * Excepción UserEmailExistsException que se lanza si existe algún usuario con un 
 * determinado email
 * @author <a href="mailto:oscarrbr@ext.inditex.com">Óscar Rodríguez Brea</a>
 *
 */
public class UserEmailExistsException extends RuntimeException{

	/**
	 * Constructor
	 * @param email String
	 */
	public UserEmailExistsException(String email) {
		super("Existe otro usuario con el mismo email = " + email);
	}
}
