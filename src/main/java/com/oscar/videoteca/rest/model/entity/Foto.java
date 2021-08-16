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
 * Clase de tipo Entity que representa a un registro de la tabla foto de BBDD
 * @author oscar
 *
 */
@Getter @Setter
@Entity
@Table(name = "foto")
public class Foto {
	    
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;
    
    @Column(nullable = false,name = "nombre")
    private String nombre;
    
    @Column(nullable = false,name = "ruta")
    private String ruta;
    
    @Column(nullable = false,name = "rutaMiniatura")
    private String rutaMiniatura;
     
    @Column(nullable = false,name = "publico")
    private String publico;
        
    @Column(nullable = false,name = "alto")
    private Integer alto;
    
    @Column(nullable = false,name = "ancho")
    private Integer ancho;
    
    @Column(nullable = true,name = "numeroVisualizaciones")
    private Integer numeroVisualizaciones;
    
    @Column(nullable = true,name = "tipoMime")
    private String tipoMime;
    
    @Column(nullable = false, updatable = false,name="fechaAlta")
    @CreationTimestamp
    private Date fechaAlta;
        
    @JoinColumn(name="idUsuario")
    @ManyToOne
    private User usuario;
    
    @JoinColumn(name="idAlbum")
    @ManyToOne
    private Album album;
    
    
	
}
