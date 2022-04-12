package com.oscar.videoteca.rest.exception;

import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ResponseStatus;


/**
 * Excepción que se lanza cuando no se encuentra un usuario
 * @author <a href="mailto:oscar.rodriguezbrea@gmail.com> </a>
 */
@ResponseStatus(HttpStatus.NOT_FOUND)
public class UserNotFoundException extends RuntimeException{

	private static final long serialVersionUID = 1L;

	/**
	 * Constructor
	 * @param id Id del usuario
	 */
	public UserNotFoundException(Long id) {
		super("No se puede encontrar el usuario con ID: " + id);
	}
	
	
	/**
	 * Constructor
	 * @param login Login del usuario
	 */
	public UserNotFoundException(String login) {
		super("No se puede encontrar el usuario con login: " + login + " y/o contraseña introducida");
	}
	
	
}
