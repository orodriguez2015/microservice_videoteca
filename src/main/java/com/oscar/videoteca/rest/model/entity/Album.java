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
 * Clase de tipo Entity que representa a un registro de la tabla users de BBDD
 * @author oscar
 *
 */
@Getter @Setter
@Entity
@Table(name = "album")
public class Album {
     
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;
    
    @Column(nullable = false,name = "nombre")
    private String nombre;
    
    @Column(nullable = false,name = "descripcion")
    private String descripcion;
     
    @Column(nullable = false,name = "publico")
    private Boolean publico;
 
    @Column(nullable = false, updatable = false,name="fecha_alta")
    @CreationTimestamp
    private Date fechaAlta;
    	    
    @Column(nullable = true, updatable = true,name="fecha_modificacion")    
    private Date fechaModificacion;
    
    @ManyToOne
    @JoinColumn(name="id_usuario")
    private User usuarioAlta;
    
    @ManyToOne
    @JoinColumn(name="id_usuario_mod")
    private User usuarioModificacion;
	
}
