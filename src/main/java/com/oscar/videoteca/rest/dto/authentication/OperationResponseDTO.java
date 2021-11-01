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
public class OperationResponseDTO implements Serializable {

    /** Código de estado/respuesta de la operación de un servicio */
	private Integer codStatus;
	/** Código de estado/respuesta de la operación de un servicio */
	private String descStatus;
	
	/** Contiene el token JWT */
	private String tokenJwt;
	
	
}
