package com.oscar.videoteca.rest.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;

import com.oscar.videoteca.rest.dto.VideotecaDTO;
import com.oscar.videoteca.rest.exception.api.ResponseError;
import com.oscar.videoteca.rest.manager.VideotecaManager;

import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;


/**
 * Controller para el manejo de videotecas
 * @author <a href="mailto:oscarrbr@ext.inditex.com">Óscar Rodríguez Brea</a>
 *
 */
@RestController
public class VideotecasController {

	@Autowired
	private VideotecaManager videotecaManager;
	

	/**
	 * Recupera un determinado usuario de la BBDD
	 * @param id Id del usuario
	 * @return UserDTO
	 */
	@GetMapping(value="/p_videoteca")
	@ApiOperation(value="Recupera laBearer eyJhbGciOiJIUzUxMiJ9.eyJqdGkiOiJ2aWRlb3RlY2FKV1QiLCJzdWIiOiJhZG1pbiIsImF1dGhvcml0aWVzIjpbIlJPTEVfVVNFUiJdLCJpYXQiOjE2MzYyMzk3OTAsImV4cCI6MTYzNjI0MDM5MH0.OeYHYkY5x_Uxq5p_5SbH5VOpj5lBeTsshdBg20QVO97RDLPuUdXiNKESBtMEMcvjaysKUa7llQmk2iVf7f6ODgs videotecas públicas",notes="Provee un mecanismo para recuperar las videotecas públicas")
	@ApiResponses(value={
		@ApiResponse(code=200,message="OK",response=VideotecaDTO.class),
		@ApiResponse(code=404,message="Not Found",response=ResponseError.class),
		@ApiResponse(code=500,message="Internal Server Error",response=ResponseError.class)
	})
	public ResponseEntity<?> getVideotecasPublicas() {

		List<VideotecaDTO> videotecas = videotecaManager.getVideotecasPublicas();

		if(videotecas==null || Boolean.TRUE.equals(videotecas.isEmpty())) {
			return ResponseEntity.notFound().build();
		}else {			 
			return ResponseEntity.ok(videotecas);
		}
	}
	
	
	
}
