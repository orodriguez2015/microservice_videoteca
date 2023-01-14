package com.oscar.videoteca.rest.exception;

/**
 * Error al publicar un vídeo
 * @author <a href="mailto:oscar.rodriguezbrea@gmail.com">Óscar Rodríguez</a>
 *
 */
public class ErrorPublishVideoException extends RuntimeException {

	private static final long serialVersionUID = -4005823987319945054L;

	/**
	 * Constructor
	 * @param message String
	 */
	public ErrorPublishVideoException(String message) {
		super(message);
	}
	
	
	/**
	 * Constructor
	 * @param message String
	 * @param t Throwable
	 */
	public ErrorPublishVideoException(String message,Throwable t) {
		super(message,t);
	}

}
