package com.oscar.videoteca.rest.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;

import com.oscar.videoteca.rest.dto.CreateVideotecaDTO;
import com.oscar.videoteca.rest.dto.VideotecaDTO;
import com.oscar.videoteca.rest.exception.ErrorDeleteVideotecaException;
import com.oscar.videoteca.rest.exception.VideotecaNotFoundException;
import com.oscar.videoteca.rest.exception.VideotecasNotFoundException;
import com.oscar.videoteca.rest.exception.api.ResponseError;
import com.oscar.videoteca.rest.exception.api.ResponseOperation;
import com.oscar.videoteca.rest.manager.VideotecaManager;
import com.oscar.videoteca.rest.util.ResourceVisibilityEnum;

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
	private VideotecaManager manager;
	

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
		List<VideotecaDTO> videotecas = manager.getVideotecasPublicas();

		if(videotecas==null || Boolean.TRUE.equals(videotecas.isEmpty())) {
			return ResponseEntity.notFound().build();
		}else {			 
			return ResponseEntity.ok(videotecas);
		}
	}

	
	/**
	 * Recupera las videotecas de un determinado usuario. Se 
	 * @param id Id del usuario
	 * @return ResponseEntity
	 */
	@GetMapping(value="/private/videotecas/{idUsuario}")
	@ApiOperation(value="Recupera las videotecas de un determinado usuario",notes="Provee un mecanismo para recuperar las videotecas de un usuario")
	@ApiResponses(value={
		@ApiResponse(code=200,message="OK",response=VideotecaDTO.class),
		@ApiResponse(code=404,message="Not Found",response=ResponseError.class),
		@ApiResponse(code=500,message="Internal Server Error",response=ResponseError.class)
	})
	public ResponseEntity<?> getVideotecasUsuario(@PathVariable Long idUsuario) throws VideotecasNotFoundException {
		List<VideotecaDTO> videotecas = manager.getVideotecasUsuario(idUsuario);
		
		// En api rest al hacer un delete se devuelve un noContent porque una vez borrado
		// el producto, este no existe en el servidor
		ResponseOperation<List<VideotecaDTO>> result = new ResponseOperation<>();
		result.setStatus(HttpStatus.OK);
		result.setData(videotecas);
		result.setDescStatus("OK");
		
		return ResponseEntity.status(HttpStatus.OK).body(result);
	}
	
	
	
	/**
	 * Recupera los vídeos de una determinada videoteca 
	 * @param idVideoteca Id de la videoteca
	 * @return ResponseEntity
	 */
	@GetMapping(value="/private/videos/{idVideoteca}")
	@ApiOperation(value="Recupera las videotecas de un determinado usuario",notes="Provee un mecanismo para recuperar las videotecas de un usuario")
	@ApiResponses(value={
		@ApiResponse(code=200,message="OK",response=VideotecaDTO.class),
		@ApiResponse(code=404,message="Not Found",response=ResponseError.class),
		@ApiResponse(code=500,message="Internal Server Error",response=ResponseError.class)
	})
	public ResponseEntity<?> getVideos(@PathVariable Long idVideoteca) throws VideotecaNotFoundException {
		VideotecaDTO videoteca = manager.getVideos(idVideoteca);
		
		// En api rest al hacer un delete se devuelve un noContent porque una vez borrado
		// el producto, este no existe en el servidor
		ResponseOperation<VideotecaDTO> result = new ResponseOperation<>();
		result.setStatus(HttpStatus.OK);
		result.setData(videoteca);
		result.setDescStatus("OK");
		
		return ResponseEntity.status(HttpStatus.OK).body(result);
	}
	
	
	
	/**
	 * Recupera una determinada videoteca de un determinado usuario para su posible edición 
	 * @param id Id del usuario
	 * @return ResponseEntity<
	 */
	@GetMapping(value="/private/videotecas/{idVideoteca}/{idUsuario}")
	@ApiOperation(value="Recupera una determinada videoteca",notes="Provee un mecanismo para recuperar una determinada videoteca")
	@ApiResponses(value={
		@ApiResponse(code=200,message="OK",response=VideotecaDTO.class),
		@ApiResponse(code=404,message="Not Found",response=ResponseError.class),
		@ApiResponse(code=500,message="Internal Server Error",response=ResponseError.class)
	})
	public ResponseEntity<?> getVideoteca(@PathVariable Long idVideoteca,@PathVariable Long idUsuario) throws VideotecaNotFoundException {
		VideotecaDTO videoteca = manager.getVideoteca(idVideoteca,idUsuario,ResourceVisibilityEnum.NONE);
		
		// En api rest al hacer un delete se devuelve un noContent porque una vez borrado
		// el producto, este no existe en el servidor
		ResponseOperation<VideotecaDTO> result = new ResponseOperation<>();
		result.setStatus(HttpStatus.OK);
		result.setData(videoteca);
		result.setDescStatus("OK");
		
		return ResponseEntity.status(HttpStatus.OK).body(result);
	}
	
	
		
	/**
	 * Persiste una videoteca en BBDD 
	 * @param album CreateVideotecaDTO que contiene la info básica de la videoteca
	 * @return ResponseEntity<?>
	 */
	@PostMapping(value="/private/videotecas")
	@ApiOperation(value="Da de alta una nueva videoteca",notes="Provee de un mecanismo para dar de alta una videoteca")
	@ApiResponses(value={
			@ApiResponse(code=200,message="OK",response=VideotecaDTO.class),
			@ApiResponse(code=401,message="Not Found",response=ResponseError.class),
			@ApiResponse(code=403,message="Not Found",response=ResponseError.class),
			@ApiResponse(code=404,message="Not Found",response=ResponseError.class),
			@ApiResponse(code=500,message="Internal Server Error",response=ResponseError.class)
		})
	public ResponseEntity<?> save(@RequestBody CreateVideotecaDTO videoteca) {
		VideotecaDTO salida = manager.save(videoteca);
		
		// En api rest al hacer un delete se devuelve un noContent porque una vez borrado
		// el producto, este no existe en el servidor
		ResponseOperation<VideotecaDTO> result = new ResponseOperation<>();
		result.setStatus(HttpStatus.CREATED);
		result.setData(salida);
		result.setDescStatus("OK");
				
		return ResponseEntity.status(HttpStatus.CREATED).body(result);
	}

	
	
	/**
	 * Persiste un álbum en BBDD 
	 * @param album AlbumDTO que contiene la info básica del álbum
	 * @return ResponseEntity<?>
	 */
	@PutMapping(value="/private/videotecas/{idVideoteca}")
	@ApiOperation(value="Actualiza una determinada videoteca",notes="Provee de un mecanismo para editar una determinada videoteca")
	@ApiResponses(value={
			@ApiResponse(code=200,message="OK",response=CreateVideotecaDTO.class),
			@ApiResponse(code=401,message="Not Found",response=ResponseError.class),
			@ApiResponse(code=403,message="Not Found",response=ResponseError.class),
			@ApiResponse(code=404,message="Not Found",response=ResponseError.class),
			@ApiResponse(code=500,message="Internal Server Error",response=ResponseError.class)
		})
	public ResponseEntity<?> updateVideoteca(@RequestBody CreateVideotecaDTO videoteca) throws VideotecaNotFoundException {
		VideotecaDTO salida = manager.update(videoteca);
		
		// En api rest al hacer un delete se devuelve un noContent porque una vez borrado
		// el producto, este no existe en el servidor
		ResponseOperation<VideotecaDTO> result = new ResponseOperation<VideotecaDTO>();
		result.setStatus(HttpStatus.CREATED);
		result.setData(salida);
		result.setDescStatus("OK");		
		return ResponseEntity.status(HttpStatus.CREATED).body(result);
	}
	
	
	
	/**
	 * Recupera una determinada videoteca de un determinado usuario para su posible edición 
	 * @param id Id del usuario
	 * @return ResponseEntity<?>
	 */
	@DeleteMapping(value="/private/videotecas/{idVideoteca}/{idUsuario}")
	@ApiOperation(value="Recupera una determinada videoteca",notes="Provee un mecanismo para recuperar una determinada videoteca")
	@ApiResponses(value={
		@ApiResponse(code=200,message="OK",response=VideotecaDTO.class),
		@ApiResponse(code=404,message="Not Found",response=ResponseError.class),
		@ApiResponse(code=500,message="Internal Server Error",response=ResponseError.class)
	})
	public ResponseEntity<?> deleteVideoteca(@PathVariable Long idVideoteca,@PathVariable Long idUsuario) throws ErrorDeleteVideotecaException,VideotecaNotFoundException {
		
		if(Boolean.FALSE.equals(manager.existsById(idVideoteca))) {
			throw new VideotecaNotFoundException("La videoteca no existe");
		}  
		
		if(Boolean.TRUE.equals(manager.delete(idVideoteca,idUsuario))){
			// En api rest al hacer un delete se devuelve un noContent porque una vez borrado
			// el producto, este no existe en el servidor
			ResponseOperation<Object> result = new ResponseOperation<>();
			result.setStatus(HttpStatus.OK);
			result.setData(null);
			result.setDescStatus("OK");
			return ResponseEntity.status(HttpStatus.OK).body(result);
		} else {
			throw new ErrorDeleteVideotecaException("Se ha producido un error al eliminar la videoteca");
		}	
	}
	
	
	
}
