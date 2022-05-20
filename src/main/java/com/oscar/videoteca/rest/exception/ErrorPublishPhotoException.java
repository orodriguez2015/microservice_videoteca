package com.oscar.videoteca.rest.exception;

/**
 * Exception que se lanza cuando se ha producido un error al publicar/despublicar una fotografía
 * @author <a href="mailto:oscar.rodriguezbrea@gmail.com">Óscar Rodríguez Brea</a>
 *
 */
public class ErrorPublishPhotoException extends Exception {
	
	private static final long serialVersionUID = -6730960287157810469L;

	/**
	 * Constructor
	 * @param message String
	 */
	public ErrorPublishPhotoException(String message) {
		super(message);
	}
	
	
	/**
	 * Constructor
	 * @param message String
	 * @param t Throwable
	 */
	public ErrorPublishPhotoException(String message,Throwable t) {
		super(message,t);
	}

}
