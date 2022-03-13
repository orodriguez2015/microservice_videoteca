package com.oscar.videoteca.rest.exception;

public class SavePhotoException extends RuntimeException {


	private static final long serialVersionUID = 2626159706325250383L;

	/**
	 * Constructor
	 * @param message String
	 */
	public SavePhotoException(String message) {
		super(message);
	}

	
	/**
	 * Constructor
	 * @param t Throwable
	 */
	public SavePhotoException(Throwable t) {
		super(t);
	}
	
	/**
	 * Constructor
	 * @param message String
	 * @param t Throwable t
	 */
	public SavePhotoException(String message,Throwable t) {
		super(message,t);
	}
}
