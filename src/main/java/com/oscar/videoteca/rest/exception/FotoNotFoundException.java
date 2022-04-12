package com.oscar.videoteca.rest.exception;


/**
 * Excepción que se lanza cuando no se han encontrado una fotografía
 * @author <a href="mailto:oscar.rodriguezbrea@gmail.com">Óscar Rodríguez Brea</a>
 *
 */
public class FotoNotFoundException extends Exception {
	
	
	/*
	 * serialVersionUID
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * Constructor
	 * @param message String
	 */
	public FotoNotFoundException(String message) {
		super(message);
	}
	
	/**
	 * Constructor
	 * @param message String
	 * @param e Exception
	 */
	public FotoNotFoundException(String message,Exception e) {
		super(message,e);
	}

}
