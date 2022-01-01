package com.oscar.videoteca.rest.exception;

/**
 * Excepción que se lanza cuando no existe un Album
 * @author <a href="mailto:oscarrbr@ext.inditex.com">Óscar Rodríguez Brea</a>
 *
 */
public class AlbumNotFoundException extends RuntimeException {
	
	/**
	 * serialVersionUID
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * Constructor
	 * @param message String
	 */
	public AlbumNotFoundException(String message) {
		super(message);
	}
	
	/**
	 * Constructor
	 * @param message String
	 * @param e Exception
	 */
	public AlbumNotFoundException(String message,Exception e) {
		super(message,e);
	}

}
