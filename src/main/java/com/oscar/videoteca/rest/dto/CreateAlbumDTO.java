package com.oscar.videoteca.rest.dto;

import java.io.Serializable;

import lombok.Getter;
import lombok.Setter;

/**
 * Clase con los datos necesarios para que se pueda dar de alta un álbum 
 * @author <a href="mailto:oscarrbr@ext.inditex.com">Óscar Rodríguez Brea</a>
 *
 */
@Getter
@Setter
public class CreateAlbumDTO implements Serializable{
	private static final long serialVersionUID = 1L;
	/** Id del álbum */
	private Long id;
	/** Nombre del álbum */
	private String nombre;
	/** Descripción del álbum */
	private String descripcion;
	/** Álbum visible para todos o no */
	private Boolean publico;
	/** Id del usuario que da de alta el usuario */
	private Long idUsuarioAlta;
	
}
