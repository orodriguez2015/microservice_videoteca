package com.oscar.videoteca.rest.exception.api;

import java.time.LocalDateTime;

import org.springframework.http.HttpStatus;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonFormat.Shape;

import lombok.Getter;
import lombok.NonNull;
import lombok.Setter;

/**
 * Clase con la respuesta de un servicio en el caso de que haya ocurrido alguna excepción.
 * Es utilizada por los métodos anotados con @ExceptionHandler
 * @author oscar
 *
 */
@Getter @Setter  //@NoArgsConstructor @RequiredArgsConstructor
public class ResponseError extends ParentResponse{

	private static final long serialVersionUID = 1L;
	@JsonFormat(shape = Shape.STRING,pattern="dd/MM/yyyy HH:mm:ss")
	private LocalDateTime date =LocalDateTime.now();
	@NonNull
	private String message;
	
	
	/**
	 * Constructor
	 * @param status
	 * @param message
	 */
	public ResponseError(HttpStatus status,String message) {
		setStatus(status);
		this.message = message;
	}
}