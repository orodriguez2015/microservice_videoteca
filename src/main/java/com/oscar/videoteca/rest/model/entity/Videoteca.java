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
@Table(name = "videoteca")
public class Videoteca {
	
	@Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;
    
    @Column(nullable = false,name = "nombre")
    private String nombre;
    
    @Column(nullable = false,name = "publico")
    private Boolean publico;
        
    @Column(nullable = false, updatable = false,name="fecha_alta")
    @CreationTimestamp
    private Date fechaAlta;
    
    @Column(nullable = true, updatable = true,name="fecha_modificacion")    
    private Date fechaModificacion;
    
    @Column(nullable = true, updatable = true,name="fecha_baja")    
    private Date fechaBaja;
        
    @JoinColumn(name="id_usuario", nullable=false,updatable=true)
    @ManyToOne
    private User usuario;    
    

}
