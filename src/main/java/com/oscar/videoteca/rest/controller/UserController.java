package com.oscar.videoteca.rest.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;

import com.oscar.videoteca.rest.dto.UserDTO;
import com.oscar.videoteca.rest.dto.mapping.UserConverter;
import com.oscar.videoteca.rest.exception.UserNotFoundException;
import com.oscar.videoteca.rest.exception.api.ResponseError;
import com.oscar.videoteca.rest.model.entity.User;
import com.oscar.videoteca.rest.model.repository.UserRepository;

import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;

/**
 * Controlador para la gestión de usuarios
 * @author <a href="mailto:oscarrbr@ext.inditex.com">Óscar Rodríguez Brea</a>
 *
 */
@RestController
public class UserController {

	@Autowired
	private UserRepository userRepository;
	
	@Autowired
	private UserConverter userConverter;
	
	
	
	/**
	 * Recupera un determinado usuario de la BBDD
	 * @param id Id del usuario
	 * @return UserDTO
	 */
	@GetMapping(value="/user/{id}")
	@ApiOperation(value="Permite recuperar un determinado usuario",notes="Provee un mecanismo para recuperar un determinado usuario")
	@ApiResponses(value={
		@ApiResponse(code=200,message="OK",response=UserDTO.class),
		@ApiResponse(code=404,message="Not Found",response=ResponseError.class),
		@ApiResponse(code=500,message="Internal Server Error",response=ResponseError.class)
	})
	public UserDTO getUser(@PathVariable Long id) {
		return userRepository.findById(id).map(user->{
			return userConverter.convertTo(user);
		}).orElseThrow(() -> new UserNotFoundException(id));
		
	}
	
	
	
	/**
	 * Recupera un determinado usuario de la BBDD
	 * @param id Id del usuario
	 * @return UserDTO
	 */
	@PostMapping(value="/user",consumes=MediaType.APPLICATION_JSON_VALUE)
	@ApiOperation(value="Permite dar de alta un usuario",notes="Provee un mecanismo para guardar un determinado usuario")
	@ApiResponses(value={
		@ApiResponse(code=200,message="OK",response=UserDTO.class),
		@ApiResponse(code=404,message="Not Found",response=ResponseError.class),
		@ApiResponse(code=500,message="Internal Server Error",response=ResponseError.class)
	})
	public ResponseEntity<UserDTO> saveUser(@RequestBody UserDTO nuevo){
		System.out.println("saveUser");
		
		User user = userRepository.save(userConverter.convertTo(nuevo));
		return ResponseEntity.status(HttpStatus.CREATED).body(userConverter.convertTo(user));	
			
	}
	
	
	
}
