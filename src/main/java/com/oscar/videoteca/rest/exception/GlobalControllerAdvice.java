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
	public ResponseEntity<ResponseError> handleUserNotFoundException(UserNotFoundException e) {
		ResponseError error = new ResponseError(HttpStatus.NOT_FOUND,e.getMessage());
		return ResponseEntity.status(HttpStatus.NOT_FOUND).body(error);
	}
	
	

	/**
	 * Manejador para excepciones de tipo UserLoginExistsException. Esto permite controlar la salida
	 * para devolver exactamente lo que se quiere devolver, y customizar la salida de Spring
	 * @param e UserLoginExistsException
	 * @return ResponseEntity<UserLoginExistsException>
	 */
	@ExceptionHandler(UserLoginExistsException.class)
	public ResponseEntity<ResponseError> handleUserLoginExistsException(UserLoginExistsException e) {
		ResponseError error = new ResponseError(HttpStatus.BAD_REQUEST,e.getMessage());
		return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(error);
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
	 * Se controla un error genérico que pueda ocurrir y que no haya sido controado
	 * @param e Exception 
	 * @return ResponseEntity<ResponseError>
	 */
	@ExceptionHandler(Exception.class)
	public ResponseEntity<ResponseError> handleException(Exception e) {
		ResponseError error = new ResponseError(HttpStatus.INTERNAL_SERVER_ERROR,e.getMessage());
		return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(error);
	}
	

	/**
	 * Se controla cuando no se ha podido recuperar un determinado álbum fotográfico
	 * @param e AlbumNotFoundException 
	 * @return ResponseEntity<ResponseError>
	 */
	@ExceptionHandler(AlbumNotFoundException.class)
	public ResponseEntity<ResponseError> handleAlbumNotFoundException(AlbumNotFoundException e) {
		ResponseError error = new ResponseError(HttpStatus.NOT_FOUND,e.getMessage());
		return ResponseEntity.status(HttpStatus.NOT_FOUND).body(error);
	}

	
	/**
	 * Se controla cuando no hay álbumes fotográficos disponibles
	 * @param e AlbumesNotFoundException 
	 * @return ResponseEntity<ResponseError>
	 */
	@ExceptionHandler(AlbumesNotFoundException.class)
	public ResponseEntity<ResponseError> handleAlbumesNotFoundException(AlbumesNotFoundException e) {
		ResponseError error = new ResponseError(HttpStatus.NOT_FOUND,e.getMessage());
		return ResponseEntity.status(HttpStatus.NOT_FOUND).body(error);
	}


	/**
	 * Se controla cuando se ha producido un error al eliminar un álbum fotográfico
	 * @param e ErrorDeleteAlbumException 
	 * @return ResponseEntity<ResponseError>
	 */
	@ExceptionHandler(ErrorDeleteAlbumException.class)
	public ResponseEntity<ResponseError> handleErrorDeleteAlbumException(ErrorDeleteAlbumException e) {
		ResponseError error = new ResponseError(HttpStatus.INTERNAL_SERVER_ERROR,e.getMessage());
		return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(error);
	}

	

	/**
	 * Se controla cuando se ha producido un error al recuperar las videotecas de un usuario
	 * @param e VideotecasNotFoundException 
	 * @return ResponseEntity<ResponseError>
	 */
	@ExceptionHandler(VideotecasNotFoundException.class)
	public ResponseEntity<ResponseError> handleVideotecasNotFoundException(VideotecasNotFoundException e) {
		ResponseError error = new ResponseError(HttpStatus.NOT_FOUND,e.getMessage());
		return ResponseEntity.status(HttpStatus.NOT_FOUND).body(error);
	}
	
	
	/**
	 * Se controla cuando se ha producido un error al recuperar una determinada videoteca
	 * @param e VideotecaNotFoundException 
	 * @return ResponseEntity<ResponseError>
	 */
	@ExceptionHandler(VideotecaNotFoundException.class)
	public ResponseEntity<ResponseError> handleVideotecaNotFoundException(VideotecaNotFoundException e) {
		ResponseError error = new ResponseError(HttpStatus.NOT_FOUND,e.getMessage());
		return ResponseEntity.status(HttpStatus.NOT_FOUND).body(error);
	}
	
	
	
	/**
	 * Se controla cuando se ha producido un error al persistir una archivo subido al servidor en disco
	 * @param e SaveFileException
	 * @return ResponseEntity<ResponseError>
	 */
	@ExceptionHandler(SaveFileException.class)
	public ResponseEntity<ResponseError> handleSaveFileException(SaveFileException e) {
		ResponseError error = new ResponseError(HttpStatus.INTERNAL_SERVER_ERROR,e.getMessage());
		return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(error);
	}
	

	/**
	 * Se controla cuando se ha producido un error al subir una fotografía a un álbum
	 * @param e SavePhotoException
	 * @return ResponseEntity<ResponseError>
	 */
	@ExceptionHandler(SavePhotoException.class)
	public ResponseEntity<ResponseError> handleSavePhotoException(SavePhotoException e) {
		ResponseError error = new ResponseError(HttpStatus.INTERNAL_SERVER_ERROR,e.getMessage());
		return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(error);
	}
	
	
	/**
	 * Se controla cuando se ha producido un error al recuperar una fotografía
	 * @param e PhotoNotFoundException
	 * @return ResponseEntity<ResponseError>
	 */
	@ExceptionHandler(PhotoNotFoundException.class)
	public ResponseEntity<ResponseError> handlePhotoNotFoundException(PhotoNotFoundException e) {
		ResponseError error = new ResponseError(HttpStatus.INTERNAL_SERVER_ERROR,e.getMessage());
		return ResponseEntity.status(HttpStatus.NOT_FOUND).body(error);
	}
	
	/**
	 * Se controla cuando se ha producido un error al recuperar una fotografía
	 * @param e PhotoNotFoundException
	 * @return ResponseEntity<ResponseError>
	 */
	@ExceptionHandler(ErrorPublishPhotoException.class)
	public ResponseEntity<ResponseError> handleErrorPublishPhotoException(ErrorPublishPhotoException e) {
		ResponseError error = new ResponseError(HttpStatus.INTERNAL_SERVER_ERROR,e.getMessage());
		return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(error);
	}

	
	/**
	 * Se controla cuando se ha producido un error al recuperar las fotografías de un álbum
	 * @param e ErrorGetPhotosAlbumException
	 * @return ResponseEntity<ResponseError>
	 */
	@ExceptionHandler(ErrorGetPhotosAlbumException.class)
	public ResponseEntity<ResponseError> handleErrorPublishPhotoException(ErrorGetPhotosAlbumException e) {
		ResponseError error = new ResponseError(HttpStatus.INTERNAL_SERVER_ERROR,e.getMessage());
		return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(error);
	}

	
	/**
	 * Se controla cuando se ha producido un error al eliminar una videoteca
	 * @param e ErrorDeleteVideotecaException
	 * @return ResponseEntity<ResponseError>
	 */
	@ExceptionHandler(ErrorDeleteVideotecaException.class)
	public ResponseEntity<ResponseError> handleErrorDeleteVideotecaException(ErrorDeleteVideotecaException e) {
		ResponseError error = new ResponseError(HttpStatus.INTERNAL_SERVER_ERROR,e.getMessage());
		return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(error);
	}
	
	
}