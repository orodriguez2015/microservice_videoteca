package com.oscar.videoteca.rest.exception;


/**
 * Excepción lanzada cuando no se ha encontrado una determinada videoteca
 * @author <a href="mailto:oscar.rodriguezbrea@gmail.com">Óscar Rodríguez Brea</a>
 *
 */
public class VideotecaNotFoundException extends Exception {
	
	/**
	 *  serialVersionUID
	 */
	private static final long serialVersionUID = 8916302007246347347L;

	/**
	 * Constructor
	 * @param message String
	 */
	public VideotecaNotFoundException(String message) {
		super(message);
	}

	/**
	 * Constructor
	 * @param message Mensaje de error
	 * @param t Throwable
	 */
	public VideotecaNotFoundException(String message,Throwable t) {
		super(message,t);
	}
}