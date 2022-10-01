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

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;


/**
 * Clase de tipo Entity que representa a un registro de la tabla users de BBDD
 * @author oscar
 *
 */
@AllArgsConstructor @NoArgsConstructor
@Getter @Setter @Builder
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
    
    // Relación OneToMany con la entidad fotos. La propiedad mappedBy se mapea el parámetro album en la entidad foto
    //@OneToMany(cascade = CascadeType.ALL,fetch = FetchType.LAZY, mappedBy = "album")
    //@OneToMany(cascade = CascadeType.ALL,mappedBy = "album")
    //private List<Photo> fotos;
	
}
