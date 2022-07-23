package com.oscar.videoteca.rest.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RestController;

import com.oscar.videoteca.rest.dto.VideotecaDTO;
import com.oscar.videoteca.rest.exception.VideotecasNotFoundException;
import com.oscar.videoteca.rest.exception.api.ResponseError;
import com.oscar.videoteca.rest.exception.api.ResponseOperation;
import com.oscar.videoteca.rest.manager.VideotecaManager;

import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;


/**
 * Controller para el manejo de videotecas
 * @author <a href="mailto:oscar.rodriguezbrea@gmail.com">Óscar Rodríguez Brea</a>
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
	@GetMapping(value="/public/videotecas")
	@ApiOperation(value="Recupera las videotecas públicas",notes="Provee un mecanismo para recuperar las videotecas públicas")
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
	
	
	
	/**
	 * Comprueba si existe una carpeta en disco con un determinado nombre y que pertenezca a un determinado usuario
	 * @param id Id del usuario propietario de las videotecas
	 * @return ResponseEntity<?>
	 * @throws VideotecasNotFoundException si no hay videotecas
	 */
	@GetMapping(value="/private/videoteca/check/{idUsuario}/{folder}")
	@ApiOperation(value="Recupera las videotecas de un determinado usuario",notes="Provee un mecanismo para recuperar los videotecas de un determinado usuario")
	@ApiResponses(value={
		@ApiResponse(code=200,message="OK",response=VideotecaDTO.class),
		@ApiResponse(code=401,message="Not Found",response=ResponseError.class),
		@ApiResponse(code=403,message="Not Found",response=ResponseError.class),
		@ApiResponse(code=404,message="Not Found",response=ResponseError.class),
		@ApiResponse(code=500,message="Internal Server Error",response=ResponseError.class)
	})
	public ResponseEntity<?> comprobarRutaVideoteca(@PathVariable Long idUsuario,@PathVariable String folder) throws VideotecasNotFoundException {
		
		Boolean exito =Boolean.FALSE;
		if(videotecaManager.checkVideoteca(idUsuario, folder)) {
			// Existe una videoteca del usuario con el mismo nombre de carpeta
			exito =Boolean.TRUE;
		}
		
		ResponseOperation<Boolean> respuesta = new ResponseOperation<>();
		respuesta.setStatus(HttpStatus.OK);
		respuesta.setData(exito);
		return ResponseEntity.status(HttpStatus.OK).body(respuesta);
	}
	
	
	
	
	
}
