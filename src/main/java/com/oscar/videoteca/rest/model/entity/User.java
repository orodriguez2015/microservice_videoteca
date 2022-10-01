package com.oscar.videoteca.rest.model.entity;

import java.io.Serializable;
import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;

import org.hibernate.annotations.CreationTimestamp;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;


/**
 * Representa un registro de la tabla users de la BBDD
 * @author <a href="mailto:oscar.rodriguezbrea@gmail.com">Óscar Rodríguez</a>
 *
 */
@AllArgsConstructor @NoArgsConstructor
@Getter @Setter @Builder
@Entity
@Table(name = "user")
public class User implements Serializable {
 
    private static final long serialVersionUID = 1L;
     
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;
    
    @Column(name = "login")
    private String login;
    
    @Column(name = "password")
    private String password;
        
    @Column(name = "nombre")
    private String nombre;
     
    @Column(name = "apellido1")
    private String apellido1;
 
    @Column(name = "apellido2")
    private String apellido2;
    
    @Column(name = "email")
    private String email;
    
    @Column(name = "root")
    private Boolean root;
    
    @Column(name = "activo")
    private Boolean activo;
    
    @Column(nullable = false, updatable = false, name="fecha_alta")
    @CreationTimestamp
    private Date fechaAlta;
    
    @Column(nullable = true, updatable = true, name="fecha_modificacion")    
    private Date fechaModificacion;
    
    @Column(nullable = true, updatable = true,name="fecha_baja")    
    private Date fechaBaja;
    
    
 
}