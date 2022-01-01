package com.oscar.videoteca.rest.exception.api;

import java.io.Serializable;

import org.springframework.http.HttpStatus;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.NonNull;
import lombok.RequiredArgsConstructor;
import lombok.Setter;


/**
 * Clase ParentResponse que representa la clase padre que se utiliza para la respuesta correcta o por error que se
 * utilizará para la respuesta de una operación de un microservicio
 * 
 * @author <a href="mailto:oscarrbr@ext.inditex.com">Óscar Rodríguez Brea</a>
 *
 */
@Getter
@Setter
@NoArgsConstructor @RequiredArgsConstructor
public class ParentResponse implements Serializable {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	
	/** Código de estado Http*/
	@NonNull
	private HttpStatus status;
	
}
