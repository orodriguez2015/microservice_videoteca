package com.oscar.videoteca.rest.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RestController;

import com.oscar.videoteca.rest.exception.api.ResponseError;
import com.oscar.videoteca.rest.exception.api.ResponseOperation;
import com.oscar.videoteca.rest.manager.VideoManager;

import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;

/**
 * Controller para el manejo de videos
 * @author <a href="mailto:oscar.rodriguezbrea@gmail.com">Óscar Rodríguez Brea</a>
 *
 */
@RestController
public class VideoController {

	@Autowired
	private VideoManager manager;
	
	/**
	 * Recupera un determinado usuario de la BBDD
	 * @param id Id del usuario
	 * @return UserDTO
	 */
	@PutMapping(value="/private/video/publish/{id}/{idUsuario}/{value}")
	@ApiOperation(value="Recupera las videotecas públicas",notes="Provee un mecanismo para recuperar las videotecas públicas")
	@ApiResponses(value={
		@ApiResponse(code=200,message="OK",response=Boolean.class),
		@ApiResponse(code=404,message="Not Found",response=ResponseError.class),
		@ApiResponse(code=500,message="Internal Server Error",response=ResponseError.class)
	})
	public ResponseEntity<?> publicVideo(@PathVariable Long id,@PathVariable Long idUsuario,@PathVariable Long value) {

		Boolean exito = this.manager.publishVideo(id, idUsuario,value);
		
		if(Boolean.TRUE.equals(exito)) {
			// Se devuelve un HTTP 200
			//return ResponseEntity.ok().build();
			
			ResponseOperation<Object> response = new ResponseOperation<Object>();
			
			response.setStatus(HttpStatus.OK);
			response.setDescStatus("OK");
			response.setData(null);
			return ResponseEntity.status(HttpStatus.OK).body(response);		
		} else {
			// Se devuelve un HTTP 500
			return ResponseEntity.internalServerError().build();
		}	
	}
}
