package com.oscar.videoteca.rest.dto;

import java.util.Date;

import lombok.Getter;
import lombok.Setter;

/**
 * Clase VideotecaDTO que contiene información sobre una determinada videoteca
 * @author <a href="mailto:oscarrbr@ext.inditex.com">Óscar Rodríguez Brea</a>
 *
 */
@Getter
@Setter
public class VideotecaDTO {

	private Long id;
    
    private String nombre;
    
    private String ruta;
    
    private String rutaCompleta;
     
    private Boolean publico;
        
    private Date fechaAlta;
       
    private Date fechaModificacion;
  
    private Date fechaBaja;
        
    //private User usuario;
    private String loginUsuario;
	
}
