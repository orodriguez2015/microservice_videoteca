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

import com.oscar.videoteca.rest.dto.AlbumDTO;
import com.oscar.videoteca.rest.dto.CreateAlbumDTO;
import com.oscar.videoteca.rest.exception.AlbumNotFoundException;
import com.oscar.videoteca.rest.exception.AlbumesNotFoundException;
import com.oscar.videoteca.rest.exception.ErrorDeleteAlbumException;
import com.oscar.videoteca.rest.exception.api.ResponseError;
import com.oscar.videoteca.rest.exception.api.ResponseOperation;
import com.oscar.videoteca.rest.manager.AlbumManager;

import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;


/**
 * Controller para el tratamiento de los álbumes fotográficos
 * @author <a href="mailto:oscar.rodriguezbrea@gmail.com">Óscar Rodríguez Brea</a>
 *
 */ 
@RestController
public class AlbumesController {

	@Autowired
	private AlbumManager manager;
	
	/**
	 * Recupera los álbumes fotográficos públicos
	 * @param id Id del usuario
	 * @return UserDTO
	 */
	@GetMapping(value="/public/album")
	@ApiOperation(value="Recupera las álbumes públicos",notes="Provee un mecanismo para recuperar los álbumes públicas")
	@ApiResponses(value={
		@ApiResponse(code=200,message="OK",response=AlbumDTO.class),
		@ApiResponse(code=404,message="Not Found",response=ResponseError.class),
		@ApiResponse(code=500,message="Internal Server Error",response=ResponseError.class)
	})
	public ResponseEntity<?> getAlbumesPublicos() {

		List<AlbumDTO> albumes = manager.getAlbumesPublicos();
R
		if(albumes==null || Boolean.TRUE.equals(albumes.isEmpty())) {
			//Si no hay álbumes se devuelve un Http 400 - Not Found
			return ResponseEntity.notFound().build();
		}else {			 
			return ResponseEntity.ok(albumes);
		}
	}
	
	
	
	/**
	 * Recupera los álbumes fotográficos públicos
	 * @param id Id del usuario
	 * @return UserDTO
	 * @throws AlbumesNotFoundException si no hay álbumes fotográficos
	 */
	@PostMapping(value="/private/albumes/{id}")
	@ApiOperation(value="Recupera las álbumes de un determinado usuario",notes="Provee un mecanismo para recuperar los álbumes de un determinado usuario")
	@ApiResponses(value={
		@ApiResponse(code=200,message="OK",response=AlbumDTO.class),
		@ApiResponse(code=401,message="Not Found",response=ResponseError.class),
		@ApiResponse(code=403,message="Not Found",response=ResponseError.class),
		@ApiResponse(code=404,message="Not Found",response=ResponseError.class),
		@ApiResponse(code=500,message="Internal Server Error",response=ResponseError.class)
	})
	public ResponseEntity<?> getAlbumesUsuario(@PathVariable Long id) throws AlbumesNotFoundException {
		List<AlbumDTO> albumes = manager.getAlbumesUsuario(id);
		
		ResponseOperation<List<AlbumDTO>> respuesta = new ResponseOperation<List<AlbumDTO>>();
		respuesta.setStatus(HttpStatus.OK);
		respuesta.setData(albumes);
		
		return ResponseEntity.status(HttpStatus.OK).body(respuesta);
	}
	

	
	
	
	/**
	 * Recupera un determinado álbum fotográfico de un detemlos álbumes fotográficos públicos
	 * @param id Id del usuario
	 * @return ResponseEntity<ResponseOperationDTO>
	 * @throws AlbumesNotFoundException si no hay álbumes fotográficos
	 */
	@GetMapping(value="/private/album/detail/{id}/{idUsuario}")
	@ApiOperation(value="Recupera las álbumes de un determinado usuario",notes="Provee un mecanismo para recuperar los álbumes de un determinado usuario")
	@ApiResponses(value={
		@ApiResponse(code=200,message="OK",response=AlbumDTO.class),
		@ApiResponse(code=401,message="Not Found",response=ResponseError.class),
		@ApiResponse(code=403,message="Not Found",response=ResponseError.class),
		@ApiResponse(code=404,message="Not Found",response=ResponseError.class),
		@ApiResponse(code=500,message="Internal Server Error",response=ResponseError.class)
	})
	public ResponseEntity<?> getAlbumUsuario(@PathVariable Long id,@PathVariable Long idUsuario) throws AlbumNotFoundException {
		
		AlbumDTO album = manager.getAlbum(id, idUsuario);
			
		ResponseOperation<AlbumDTO> respuesta = new ResponseOperation<AlbumDTO>();
		respuesta.setStatus(HttpStatus.OK);
		respuesta.setData(album);
			
		return ResponseEntity.status(HttpStatus.OK).body(respuesta);
	}
	
	
	
	
	/**
	 * Persiste un álbum en BBDD 
	 * @param album AlbumDTO que contiene la info básica del álbum
	 * @return ResponseEntity<?>
	 */
	@PostMapping(value="/private/album")
	@ApiOperation(value="Da de alta un nuevo álbum fotográfico",notes="Provee de un mecanismo para dar de alta álbumes fotográficos")
	@ApiResponses(value={
			@ApiResponse(code=200,message="OK",response=AlbumDTO.class),
			@ApiResponse(code=401,message="Not Found",response=ResponseError.class),
			@ApiResponse(code=403,message="Not Found",response=ResponseError.class),
			@ApiResponse(code=404,message="Not Found",response=ResponseError.class),
			@ApiResponse(code=500,message="Internal Server Error",response=ResponseError.class)
		})
	public ResponseEntity<?> saveAlbum(@RequestBody CreateAlbumDTO album) {
		AlbumDTO salida = manager.saveAlbum(album);
		
		// En api rest al hacer un delete se devuelve un noContent porque una vez borrado
		// el producto, este no existe en el servidor
		ResponseOperation<AlbumDTO> result = new ResponseOperation<AlbumDTO>();
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
	@PutMapping(value="/private/album")
	@ApiOperation(value="Actualiza un determinado álbum fotográfico",notes="Provee de un mecanismo para editar un álbum fotográfico")
	@ApiResponses(value={
			@ApiResponse(code=200,message="OK",response=AlbumDTO.class),
			@ApiResponse(code=401,message="Not Found",response=ResponseError.class),
			@ApiResponse(code=403,message="Not Found",response=ResponseError.class),
			@ApiResponse(code=404,message="Not Found",response=ResponseError.class),
			@ApiResponse(code=500,message="Internal Server Error",response=ResponseError.class)
		})
	public ResponseEntity<?> updateAlbum(@RequestBody CreateAlbumDTO album) {
		AlbumDTO salida = manager.updateAlbum(album);
		
		
		// En api rest al hacer un delete se devuelve un noContent porque una vez borrado
		// el producto, este no existe en el servidor
		ResponseOperation<AlbumDTO> result = new ResponseOperation<AlbumDTO>();
		result.setStatus(HttpStatus.CREATED);
		result.setData(salida);
		result.setDescStatus("OK");
				
		return ResponseEntity.status(HttpStatus.CREATED).body(result);
	}
	
	
	
	/**
	 * Permite borrar un álbum en BBDD 
	 * @param id Id del álbum a borrar
	 * @param idUsuario Id del usuario al que pertenece el álbum
	 * @return ResponseEntity<?>
	 */
	@DeleteMapping(value="/private/album/{id}/{idUsuario}")
	@ApiOperation(value="Elimina un álbum fotográfico",notes="Provee de un mecanismo para eliminar álbumes fotográficos")
	@ApiResponses(value={
			@ApiResponse(code=200,message="OK",response=ResponseError.class),
			@ApiResponse(code=401,message="Not Found",response=ResponseError.class),
			@ApiResponse(code=403,message="Not Found",response=ResponseError.class),
			@ApiResponse(code=404,message="Not Found",response=ResponseError.class),
			@ApiResponse(code=500,message="Internal Server Error",response=ResponseError.class)
		})
	public ResponseEntity<?> deleteAlbum(@PathVariable Long id, @PathVariable Long idUsuario) throws ErrorDeleteAlbumException {
		
		if(Boolean.FALSE.equals(this.manager.existsById(id))) {
			// No existe el id del álbum a eliminar
			throw new AlbumNotFoundException("No existe el álbum a eliminar");	
		}
		
		
		if(Boolean.TRUE.equals(this.manager.deleteAlbum(id,idUsuario))) {
			// En api rest al hacer un delete se devuelve un noContent porque una vez borrado
			// el producto, este no existe en el servidor
			ResponseOperation<Object> response = new ResponseOperation<Object>();
			response.setStatus(HttpStatus.OK);
			response.setDescStatus("OK");
			response.setData(null);
			return ResponseEntity.status(HttpStatus.OK).body(response);			
		} else {
			throw new ErrorDeleteAlbumException("Error al eliminar el álbum");
		}
		
	}
	
	
	
	
}