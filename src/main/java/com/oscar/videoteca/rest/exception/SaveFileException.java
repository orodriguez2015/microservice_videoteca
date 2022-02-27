package com.oscar.videoteca.rest.exception;

/**
 * Excepción que se lanza cuando ocurre alǵun error al guardar un fichero en disco
 * @author <a href="mailto:oscar.rodriguezbrea@gmail.com">Óscar Rodríguez Brea</a>
 *
 */
public class SaveFileException extends Exception{

	/**
	 * 
	 */
	private static final long serialVersionUID = 1989911098649984844L;

	/**
	 * Constructor
	 * @param message String
	 */
	public SaveFileException(String message) {
		super(message);
	}
	

	
	/**
	 * Constructor
	 * @param message String
	 * @param e Exception
	 */
	public SaveFileException(String message,Throwable e) {
		super(message,e);
	}
	
	
	/**
	 * Constructor
	 * @param message Throwable
	 */
	public SaveFileException(Throwable e) {
		super(e);
	}


	
}
