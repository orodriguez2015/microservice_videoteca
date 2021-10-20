package com.oscar.videoteca.rest.dto.authentication;

import java.io.Serializable;

import lombok.Getter;
import lombok.Setter;

/**
 * Clase que contendrá los códigos de respuesta de una operación de un servicio
 * @author <a href="mailto:oscarrbr@ext.inditex.com">Óscar Rodríguez Brea</a>
 *
 */
@Getter
@Setter
//@Builder
public class OperationResponseDTO implements Serializable {

//	public static enum Status implements Serializable {
//		
//		OK(0),
//		USER_NOT_EXISTS(1),
//		ERROR(2);
//		@
//		private Integer codigo;
//		
//		/**
//		 * Constructor
//		 * @param codigo
//		 */
//		private Status(Integer codigo) {
//			this.codigo = codigo;
//		}
//		
//	}
//	
	private static final long serialVersionUID = 1L;
    /** Código de estado/respuesta de la operación de un servicio */
	private Integer codStatus;
	/** Código de estado/respuesta de la operación de un servicio */
	private String descStatus;
	
	
}
