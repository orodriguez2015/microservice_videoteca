package com.oscar.videoteca.rest.exception;

/**
 * Excepción lanzada cuando no se han recuperado videotecas
 * @author <a href="mailto:oscar.rodriguezbrea@gmail.com">Óscar Rodríguez Brea</a>
 *
 */
public class VideotecasNotFoundException extends Exception {
	
	/**
	 *  serialVersionUID
	 */
	private static final long serialVersionUID = 8916302007246347347L;

	/**
	 * Constructor
	 * @param message String
	 */
	public VideotecasNotFoundException(String message) {
		super(message);
	}

	/**
	 * Constructor
	 * @param message Mensaje de error
	 * @param t Throwable
	 */
	public VideotecasNotFoundException(String message,Throwable t) {
		super(message,t);
	}
}
