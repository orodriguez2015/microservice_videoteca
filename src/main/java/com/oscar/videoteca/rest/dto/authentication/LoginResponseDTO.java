package com.oscar.videoteca.rest.dto.authentication;

import java.io.Serializable;

import com.oscar.videoteca.rest.dto.UserLoginDTO;

import lombok.Getter;
import lombok.Setter;

/**
 * Clase que contendrá los códigos de respuesta de una operación de un servicio
 * @author <a href="mailto:oscar.rodriguezbrea@gmail.com">Óscar Rodríguez Brea</a>
 *
 */
@Getter
@Setter
public class LoginResponseDTO implements Serializable {

    /**
	 * serialVersionUID
	 */
	private static final long serialVersionUID = 722066133643744313L;
	/** Código de estado/respuesta de la operación de un servicio */
	private Integer codStatus;
	/** Código de estado/respuesta de la operación de un servicio */
	private String descStatus;
	/** Atributo que contiene los datos del usuario */
	private UserLoginDTO user;
	
}
