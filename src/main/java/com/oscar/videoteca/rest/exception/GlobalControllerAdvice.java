package com.oscar.videoteca.rest.exception;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;

import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.oscar.videoteca.rest.exception.api.ResponseError;

/**
 * Controlador genérico de excepciones que controla la salida en función del tipo de excepción, en lugar
 * de tener que hacerlo en cada controller 
 * @author <a href="mailto:oscar.rodriguezbrea@gmail.com">Óscar Rodríguez</a>
 *
 */
@RestControllerAdvice
public class GlobalControllerAdvice {

	/**
	 * Manejador para excepciones de tipo UserNotFoundException. Esto permite controlar la salida
	 * para devolver exactamente lo que se quiere devolver, y customizar la salida de Spring
	 * @param e UserNotFoundException
	 * @return ResponseEntity<UserNotFoundException>
	 */
	@ExceptionHandler(UserNotFoundException.class)
	public ResponseEntity<ResponseError> handleProductoNotFoundException(UserNotFoundException e) {
		ResponseError error = new ResponseError(HttpStatus.NOT_FOUND,e.getMessage());
		return ResponseEntity.status(HttpStatus.NOT_FOUND).body(error);
	}
	

	/**
	 * Manejador para excepciones de tipo JsonMappingException. Esto permite controlar la salida
	 * para devolver exactamente lo que se quiere devolver, y customizar la salida de Spring
	 * @param e ProductoNotFoundException
	 * @return ResponseEntity<ApiError>
	 */
	@ExceptionHandler(JsonMappingException.class)
	public ResponseEntity<ResponseError> handleJsonMappingException(JsonMappingException e) {
		ResponseError error = new ResponseError(HttpStatus.BAD_REQUEST,e.getMessage());
		return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(error);
	}

	
	/**
	 * Manejador para excepciones de tipo JsonMappingException. Esto permite controlar la salida
	 * para devolver exactamente lo que se quiere devolver, y customizar la salida de Spring
	 * @param e ProductoNotFoundException
	 * @return ResponseEntity<ApiError>
	 */
	@ExceptionHandler(JsonParseException.class)
	public ResponseEntity<ResponseError> handleJsonMappingException(JsonParseException e) {
		ResponseError error = new ResponseError(HttpStatus.BAD_REQUEST,e.getMessage());
		return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(error);
	}
	
	
	/**
	 * Se controla un 
	 * @param e
	 * @return
	 */
	@ExceptionHandler(Exception.class)
	public ResponseEntity<ResponseError> handleException(Exception e) {
		ResponseError error = new ResponseError(HttpStatus.INTERNAL_SERVER_ERROR,e.getMessage());
		return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(error);
	}
	
}
