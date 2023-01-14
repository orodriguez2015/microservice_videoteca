package com.oscar.videoteca.rest.exception;


/**
 * Excepción que se lanza cuando no existe un determinado vídeo
 * @author <a href="mailto:oscar.rodriguezbrea@gmail.com">Óscar Rodríguez</a>
 *
 */
public class VideoNotFoundException extends RuntimeException {
	
	private static final long serialVersionUID = -1844497976936402810L;

	public VideoNotFoundException() {
		super();
	}
	
	public VideoNotFoundException(String message) {
		super(message);
	}
	
	public VideoNotFoundException(String message,Throwable t) {
		super(message,t);
	}

}
