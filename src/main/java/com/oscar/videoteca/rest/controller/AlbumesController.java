package com.oscar.videoteca.rest.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;

import com.oscar.videoteca.rest.dto.AlbumDTO;
import com.oscar.videoteca.rest.dto.CreateAlbumDTO;
import com.oscar.videoteca.rest.exception.api.ResponseError;
import com.oscar.videoteca.rest.manager.AlbumManager;

import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;


/**
 * Controller para el tratamiento de los álbumes fotográficos
 * @author <a href="mailto:oscarrbr@ext.inditex.com">Óscar Rodríguez Brea</a>
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
	 */
	@PostMapping(value="/private/album/{id}")
	@ApiOperation(value="Recupera las álbumes de un determinado usuario",notes="Provee un mecanismo para recuperar los álbumes de un determinado usuario")
	@ApiResponses(value={
		@ApiResponse(code=200,message="OK",response=AlbumDTO.class),
		@ApiResponse(code=401,message="Not Found",response=ResponseError.class),
		@ApiResponse(code=403,message="Not Found",response=ResponseError.class),
		@ApiResponse(code=404,message="Not Found",response=ResponseError.class),
		@ApiResponse(code=500,message="Internal Server Error",response=ResponseError.class)
	})
	public ResponseEntity<?> getAlbumesUsuario(@PathVariable Long id) {

		List<AlbumDTO> albumes = manager.getAlbumesUsuario(id);

		if(albumes==null || Boolean.TRUE.equals(albumes.isEmpty())) {
			//Si no hay álbumes se devuelve un Http 400 - Not Found
			return ResponseEntity.notFound().build();
		}else {			 
			return ResponseEntity.ok(albumes);
		}
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
		return ResponseEntity.status(HttpStatus.CREATED).body(salida);
	}
	
}
