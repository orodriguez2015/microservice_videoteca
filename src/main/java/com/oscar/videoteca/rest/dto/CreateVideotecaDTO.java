package com.oscar.videoteca.rest.dto;

import java.io.Serializable;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class CreateVideotecaDTO implements Serializable {

	private static final long serialVersionUID = -2991917985700771260L;
	/** Id de la videoteca */
	private Long id;
	/** Nombre de la videoteca */
	private String nombre;	
    /** Indica si la videoteca es visible para todos los públicos */
	private Boolean publico;
	/** Id del usuario que da de alta la videoteca */
	private Long idUsuario;
}
