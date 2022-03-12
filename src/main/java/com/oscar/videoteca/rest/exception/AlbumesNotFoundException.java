package com.oscar.videoteca.rest.exception;


/**
 * Excepción que se lanza en el caso de no haya álbumes fotográficos
 * @author <a href="mailto:oscar.rodriguezbrea@gmail.com">Óscar Rodríguez Brea</a>
 *
 */
public class AlbumesNotFoundException extends Exception {

	/*
	 * serialVersionUID
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * Constructor
	 * @param message String
	 */
	public AlbumesNotFoundException(String message) {
		super(message);
	}
	
	/**
	 * Constructor
	 * @param message String
	 * @param e Exception
	 */
	public AlbumesNotFoundException(String message,Exception e) {
		super(message,e);
	}
}
