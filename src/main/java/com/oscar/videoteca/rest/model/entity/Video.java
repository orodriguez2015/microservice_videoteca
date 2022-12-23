package com.oscar.videoteca.rest.model.entity;

import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import org.hibernate.annotations.CreationTimestamp;

import lombok.Getter;
import lombok.Setter;

/**
 * Entidad Video
 * @author <a href="mailto:oscar.rodriguezbrea@gmail.com">Óscar Rodríguez</a>
 *
 */
@Getter @Setter
@Entity
@Table(name = "video")
public class Video {

	@Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;
    
    @Column(nullable = false,name = "nombre")
    private String nombre;
    
    @Column(nullable = false,name = "ruta")
    private String ruta;
    
    @Column(nullable = false,name = "publico")
    private Boolean publico;
            
    @Column(nullable = false, updatable = false,name="fecha_alta")
    @CreationTimestamp
    private Date fechaAlta;
        
    @JoinColumn(name="id_usuario", nullable=false,updatable=true)
    @ManyToOne
    private User usuario;
    
    @JoinColumn(name="id_videoteca", nullable=false,updatable=true)
    @ManyToOne
    private Videoteca videoteca;
	
}
