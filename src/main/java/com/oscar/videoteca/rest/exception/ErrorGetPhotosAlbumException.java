package com.oscar.videoteca.rest.exception;

/**
 * Excepción que se lanza cuando se ha producido un error al recuperar las fotos de un álbum
 * @author <a href="mailto:oscar.rodriguezbrea@gmail.com">Óscar Rodríguez Brea</a>
 *
 */
public class ErrorGetPhotosAlbumException extends RuntimeException {

	private static final long serialVersionUID = 9047029471454596578L;

	/**
	 * Constructor
	 * @param message String
	 */
	public ErrorGetPhotosAlbumException(String message) {
		super(message);
	}
	
	/**
	 * Constructor
	 * @param message String
	 * @param t Throwable
	 */
	public ErrorGetPhotosAlbumException(String message,Throwable t) {
		super(message,t);
	}
	
}
