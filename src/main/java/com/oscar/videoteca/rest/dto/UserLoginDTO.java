package com.oscar.videoteca.rest.dto;

import java.io.Serializable;

import io.swagger.annotations.ApiParam;
import lombok.Getter;
import lombok.Setter;

/**
 * Clase con los datos del usuario para
 * @author <a href="mailto:oscarrbr@ext.inditex.com">Óscar Rodríguez Brea</a>
 *
 */
@Getter
@Setter
public class UserLoginDTO implements Serializable {

	private static final long serialVersionUID = -7751912357066787824L;

	@ApiParam(value="Id del usuario")
    private Long id;
    
	@ApiParam(value="Login del usuario")
    private String login;
	
	@ApiParam(value="Nombre del usuario")
    private String nombre;

	@ApiParam(value="Primer apellido")
    private String apellido1;
 
	@ApiParam(value="Segundo apellido")
    private String apellido2;
    
	@ApiParam(value="Email del usuario")
    private String email;
	
	@ApiParam(value="Token JWT si el usuario está autenticado")
	private String authenticationToken;
	
}
