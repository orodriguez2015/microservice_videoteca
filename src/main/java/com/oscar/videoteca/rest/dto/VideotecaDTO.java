package com.oscar.videoteca.rest.dto;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

import lombok.Getter;
import lombok.Setter;

/**
 * Clase VideotecaDTO que contiene información sobre una determinada videoteca
 * @author <a href="mailto:oscar.rodriguezbrea@gmail.com">Óscar Rodríguez Brea</a>
 *
 */
@Getter
@Setter
public class VideotecaDTO implements Serializable{

	private static final long serialVersionUID = 6290523579825421361L;

	private Long id;
    
    private String nombre;
    
    private String ruta;
    
    private String rutaRelativa;
     
    private Boolean publico;
        
    private Date fechaAlta;
       
    private Date fechaModificacion;
  
    private Date fechaBaja;
        
    private String loginUsuario;
	
    private List<VideoDTO> videos;
}
