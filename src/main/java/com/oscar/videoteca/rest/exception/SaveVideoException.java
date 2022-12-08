package com.oscar.videoteca.rest.exception;

/**
 * Excepción que se lanza cuando ocurre un error al persistir un vídeo
 * @author <a href="mailto:oscar.rodriguezbrea@gmail.com">Óscar Rodríguez Brea</a>
 *
 */
public class SaveVideoException extends Exception {
	
	private static final long serialVersionUID = -5424866380330519308L;

	/**
	 * Constructor
	 * @param message String
	 */
	public SaveVideoException(String message) {
		super(message);
	}

	
	/**
	 * Constructor
	 * @param t Throwable
	 */
	public SaveVideoException(Throwable t) {
		super(t);
	}
	
	/**
	 * Constructor
	 * @param message String
	 * @param t Throwable t
	 */
	public SaveVideoException(String message,Throwable t) {
		super(message,t);
	}
}
