package com.oscar.videoteca.rest.dto;

import java.io.Serializable;
import java.util.Date;

import lombok.Getter;
import lombok.Setter;

/**
 * Clase VideoDTO
 * @author <a href="mailto:oscar.rodriguezbrea@gmail.com">Óscar Rodríguez Brea</a>
 *
 */
@Getter
@Setter
public class VideoDTO implements Serializable{

    private static final long serialVersionUID = -4090425952576380793L;

	private Long id;
    
    private String nombre;
    
    private String ruta;
    
    private Boolean publico;
           
    private Date fechaAlta;
        
    private Integer idUsuario;
   
    private Long idVideoteca;
}
