package com.oscar.videoteca.rest.exception;

/**
 * Constructor
 * @author <a href="mailto:oscar.rodriguezbrea@gmail.com">Óscar</a>
 *
 */
public class ErrorDeleteVideotecaException extends Exception{

	private static final long serialVersionUID = 3839694479185459288L;
	
	/**
	 * Constructor
	 */
	public ErrorDeleteVideotecaException() {
		
	}
	
	/**
	 * Constructor
	 * @param t Throwable
	 */
	public ErrorDeleteVideotecaException(Throwable t) {
		super(t);
	}
	
	/**
	 * Constructor
	 * @param message String
	 */
	public ErrorDeleteVideotecaException(String message) {
		super(message);
	}
	
	/**
	 * Constructor
	 * @param message String
	 * @param t Throwable
	 */
	public ErrorDeleteVideotecaException(String message,Throwable t) {
		super(message,t);
	}
	
}
