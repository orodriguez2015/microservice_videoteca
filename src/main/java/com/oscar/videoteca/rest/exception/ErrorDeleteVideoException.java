package com.oscar.videoteca.rest.exception;

/**
 * Exception que se lanza cuando se produce un error al eliminar un vídeo
 * @author oscar
 *
 */
public class ErrorDeleteVideoException extends RuntimeException{

	private static final long serialVersionUID = 9074867109260178568L;

	/**
	 * Constructor
	 */
	public ErrorDeleteVideoException() {
		super();
	}
	
	
	/**
	 * Constructor
	 * @param message String
	 */
	public ErrorDeleteVideoException(String message) {
		super(message);
	}
	
	
	/**
	 * Constructor
	 * @param message String
	 * @param t Throwable
	 */
	public ErrorDeleteVideoException(String message,Throwable t) {
		super(message,t);
	}
	
	
}
