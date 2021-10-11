package com.oscar.videoteca.rest.dto;

import java.io.Serializable;

import io.swagger.annotations.ApiParam;
import lombok.Getter;
import lombok.Setter;

/**
 * Clase UserDTO
 * @author <a href="mailto:oscar.rodriguezbrea@gmail.com">Óscar Rodríguez</a>
 *
 */
@Getter
@Setter
public class UserDTO implements Serializable{

	private static final long serialVersionUID = 1L;     
	 
	@ApiParam(value="Id del usuario")
    private Long id;
    
	@ApiParam(value="Login del usuario")
    private String login;

	@ApiParam(value="Password del usuario")	
	private String password;
	
	@ApiParam(value="Nombre del usuario")
    private String nombre;

	@ApiParam(value="Primer apellido")
    private String apellido1;
 
	@ApiParam(value="Segundo apellido")
    private String apellido2;
    
	@ApiParam(value="Email del usuario")
    private String email;
	
	@ApiParam(value="Indica si el usuario está o no activo")
	private Boolean activo;
}
