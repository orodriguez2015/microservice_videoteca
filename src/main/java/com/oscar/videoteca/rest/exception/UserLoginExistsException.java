package com.oscar.videoteca.rest.exception;


/**
 * Excepción que se lanza cuando existe otro usuario con el mismo login a la
 * hora de dar de alta un nuevo usuario
 * @author <a href="mailto:oscar.rodriguezbrea@gmail.com">Óscar Rodríguez Brea</a>
 *
 */
public class UserLoginExistsException extends RuntimeException {
	
	/**
	 * Constructor
	 * @param login String
	 */
	public UserLoginExistsException(String login) {
		super("Existe otro usuario con el mismo login = " + login);
	}

}
