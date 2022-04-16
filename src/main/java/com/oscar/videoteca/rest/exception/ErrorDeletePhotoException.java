package com.oscar.videoteca.rest.exception;

/**
 * Excepción que se lanza si ha ocurrido algún error al eliminar una fotografía
 * 
 * @author <a href="mailto:oscar.rodriguezbrea@gmail.com">Óscar Rodríguez Brea</a>
 */
public class ErrorDeletePhotoException extends Exception{

	private static final long serialVersionUID = -2067379826582274904L;

	/**
	 * Constructor
	 * @param message String
	 * 
	 */
	public ErrorDeletePhotoException(String message) {
		super(message);
	}
	
	/**
	 * Constructor
	 * @param message String 
	 * @param t Throwable
	 */
	public ErrorDeletePhotoException(String message,Throwable t) {
		super(message,t);
	}

	
}
