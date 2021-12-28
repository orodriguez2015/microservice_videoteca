package com.oscar.videoteca.rest.dto;

import java.io.Serializable;
import java.util.Date;

import com.oscar.videoteca.constants.ConstantsAuthentication;

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
    
    
    /**
     * Devuelve el nombre comppleto del usuario que ha dado de alta el álbum
     * @return String
     */
    public String getNombreCompleto() {
    	return this.nombreUsuarioAlta.concat(ConstantsAuthentication.BLANK_SPACE).concat(this.apellido1UsuarioAlta).concat(ConstantsAuthentication.BLANK_SPACE).concat(this.apellido2UsuarioAlta);
    }
    
}

