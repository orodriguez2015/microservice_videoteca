package com.oscar.videoteca.rest.dto;

import java.io.Serializable;
import java.util.Date;

import lombok.Getter;
import lombok.Setter;

/**
 * Clase AlbumDTO
 * @author <a href="mailto:oscarrbr@ext.inditex.com">Óscar Rodríguez Brea</a>
 *
 */
@Getter
@Setter
public class AlbumDTO implements Serializable {
 
	private static final long serialVersionUID = 1L;

	private Long id;
    
    private String nombre;
    
    private String descripcion;
     
    private Boolean publico;
 
    private Date fechaAlta;
    	    
    private Date fechaModificacion;
    
    private String loginUsuarioAlta;
    
    private String nombreUsuarioAlta;
    
    private String apellido1UsuarioAlta;

    private String apellido2UsuarioAlta;
       
}

