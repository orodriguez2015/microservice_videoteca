package com.oscar.videoteca.rest.dto;

import java.io.Serializable;
import java.text.SimpleDateFormat;
import java.util.Date;

import com.oscar.videoteca.constants.Constants;

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
public class PhotoDTO implements Serializable {

	private static final long serialVersionUID = 1L;

	private Long id;
    
    private String nombre;
    
    private String ruta;
    
    private String rutaRelativa;
    
    private String rutaRelativaMiniatura;
         
    private Integer publico;
        
    private Integer alto;
    
    private Integer ancho;
    
    private Integer numeroVisualizaciones;
    
    private String tipoMime;
    
    private Date fechaAlta;
    
    private Long idAlbum;
    
    /**
     * Devuelve la fecha de alta en un formato dd/MM/yyyy HH:mm:ss
     * @return String
     */
    public String getFechaAltaFormato() {
    	String date = "";
    	if(this.fechaAlta!=null) {
    		SimpleDateFormat sf = new SimpleDateFormat(Constants.PATTERN_DD_MM_YYYY);
    		date = sf.format(this.fechaAlta);
    	}
    	return date;
    }
  
}
