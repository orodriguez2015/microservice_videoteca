package com.oscar.videoteca.rest.exception;

/**
 * Excepción que se lanza si ha ocurrido algún error al 
 * eliminar un álbum fotográfico
 * @author <a href="mailto:oscarrbr@ext.inditex.com">Óscar Rodríguez Brea</a>
 *
 */
public class ErrorDeleteAlbumException extends Exception {
	
	private static final long serialVersionUID = -4899798914616244602L;

	/**
	 * Constructor
	 * @param message String
	 * 
	 */
	public ErrorDeleteAlbumException(String message) {
		super(message);
	}
	
	/**
	 * Constructor
	 * @param message String 
	 * @param t Throwable
	 */
	public ErrorDeleteAlbumException(String message,Throwable t) {
		super(message,t);
	}

}
