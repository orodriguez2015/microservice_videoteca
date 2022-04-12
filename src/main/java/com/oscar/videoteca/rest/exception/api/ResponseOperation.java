package com.oscar.videoteca.rest.exception.api;

import lombok.Getter;
import lombok.Setter;

/**
 * Calse ResponseOperation
 * @author <a href="mailto:oscar.rodriguezbrea@gmail.com">Óscar Rodríguez Brea</a>
 *
 */
@Getter
@Setter
public class ResponseOperation<T> extends ParentResponse {

	private static final long serialVersionUID = 1L;

	/** Descripción del estado de la operaciónm */
	private String descStatus;
	
	/** Objeto a devolver. No obligatorio */
	private T data;
}
