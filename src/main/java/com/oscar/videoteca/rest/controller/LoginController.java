package com.oscar.videoteca.rest.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;

import com.oscar.videoteca.rest.dto.LoginDTO;
import com.oscar.videoteca.rest.dto.authentication.OperationResponseDTO;
import com.oscar.videoteca.rest.exception.api.ResponseError;
import com.oscar.videoteca.rest.manager.UserManager;

import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;

/**
 * Clase LoginController que se encarga del login de un usuario en el sistema
 * @author <a href="mailto:oscarrbr@ext.inditex.com">Óscar Rodríguez Brea</a>
 *
 */
@RestController
public class LoginController {

	@Autowired
	private UserManager userManager;
	
	/**
	 * Autentica un usuario en el sistema contra la BBDD
	 * @param id Id del usuario
	 * @return UserDTO
	 */
	@PostMapping(value="/login",consumes=MediaType.APPLICATION_JSON_VALUE)
	@ApiOperation(value="Permite autenticar un usuario contra BBDD",notes="Provee un mecanismo para autenticar a un usuario")
	@ApiResponses(value={
		@ApiResponse(code=200,message="OK",response=LoginDTO.class),
		@ApiResponse(code=404,message="Not Found",response=ResponseError.class),
		@ApiResponse(code=500,message="Internal Server Error",response=ResponseError.class)
	})
	public ResponseEntity<OperationResponseDTO> autenticarUsuario(@RequestBody LoginDTO nuevo){		
		OperationResponseDTO respuesta = userManager.validarUsuario(nuevo);	
		return ResponseEntity.status(HttpStatus.OK).body(respuesta);
	}

	
		
}
