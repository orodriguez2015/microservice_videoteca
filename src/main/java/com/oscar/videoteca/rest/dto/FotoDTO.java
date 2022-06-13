package com.oscar.videoteca.rest.dto;

import java.io.Serializable;
import java.util.Date;

import lombok.Getter;
import lombok.Setter;

/**
 * Clase FotoDTO que representa a una fotografía de un determinado álbum fotográfico
 * 
 * @author <a href="mailto:oscar.rodriguezbrea@gmail.com">Óscar Rodríguez Brea</a>
 *
 */
@Getter
@Setter
public class FotoDTO implements Serializable {

	private static final long serialVersionUID = 1L;

	private Long id;
    
    private String nombre;
    
    private String ruta;
    
    private String rutaRelativa;
         
    private Integer publico;
        
    private Integer alto;
    
    private Integer ancho;
    
    private Integer numeroVisualizaciones;
    
    private String tipoMime;
    
    private Date fechaAlta;
    
    private Long idAlbum;
    
  
}
