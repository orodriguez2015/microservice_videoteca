package com.oscar.videoteca.rest.dto;

import java.io.Serializable;
import java.text.SimpleDateFormat;
import java.util.Date;

import com.oscar.videoteca.constants.Constants;

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
    
    private String rutaRelativa;
    
    private Boolean publico;
           
    private Date fechaAlta;
        
    private Integer idUsuario;
   
    private Long idVideoteca;
    
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
