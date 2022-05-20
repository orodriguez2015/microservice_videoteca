package com.oscar.videoteca.rest.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RestController;

import com.oscar.videoteca.rest.exception.ErrorDeletePhotoException;
import com.oscar.videoteca.rest.exception.ErrorPublishPhotoException;
import com.oscar.videoteca.rest.exception.PhotoNotFoundException;
import com.oscar.videoteca.rest.exception.api.ResponseError;
import com.oscar.videoteca.rest.exception.api.ResponseOperation;
import com.oscar.videoteca.rest.manager.PhotoManager;

import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;


/**
 * Controller para el tratamiento de las fotografías
 * 
 * @author <a href="mailto:oscar.rodriguezbrea@gmail.com">Óscar Rodríguez Brea</a>
 *
 */ 
@RestController
public class PhotoController {
	
	@Autowired
	private PhotoManager manager;
	
	/**
	 * Permite borrar una determinada fotografía de un álbum.
	 * El borrado es físico, se elimina tanto de la BBDD como de espacio en disco en el que
	 * esté almacenada la fotografía 
	 * @param idPhoto Id de la fotografía a eliminar
	 * @param idUsuario Id del usuario al que pertenece el álbum
	 * @return ResponseEntity<?>
	 */
	@DeleteMapping(value="/private/photo/{idPhoto}/{idUsuario}")
	@ApiOperation(value="Elimina un fotografía",notes="Provee de un mecanismo para eliminar una fotografía")
	@ApiResponses(value={
			@ApiResponse(code=200,message="OK",response=ResponseError.class),
			@ApiResponse(code=401,message="Not Found",response=ResponseError.class),
			@ApiResponse(code=403,message="Not Found",response=ResponseError.class),
			@ApiResponse(code=404,message="Not Found",response=ResponseError.class),
			@ApiResponse(code=500,message="Internal Server Error",response=ResponseError.class)
		})
	public ResponseEntity<?> deletePhoto(@PathVariable Long idPhoto, @PathVariable Long idUsuario) throws PhotoNotFoundException,ErrorDeletePhotoException {
		
		if(Boolean.FALSE.equals(this.manager.getPhoto(idPhoto))) {
			// No existe el id del álbum a eliminar
			throw new PhotoNotFoundException("No existe la fotografía a eliminar");	
		}
		
		if(Boolean.TRUE.equals(this.manager.deletePhoto(idPhoto,idUsuario))) {
			// En api rest al hacer un delete se devuelve un noContent porque una vez borrado
			// el producto, este no existe en el servidor
			ResponseOperation<Object> response = new ResponseOperation<Object>();
			response.setStatus(HttpStatus.OK);
			response.setDescStatus("OK");
			response.setData(null);
			return ResponseEntity.status(HttpStatus.OK).body(response);			
		} else {
			throw new ErrorDeletePhotoException("Error al eliminar la fotografía");
		}
		
	}
	
	
	/**
	 * Permite publicar/despublicar una determinada fotografía de un álbum.
	 * 
	 * @param idPhoto Id de la fotografía a eliminar
	 * @param idUsuario Id del usuario al que pertenece el álbum
	 * @param 
	 * @return ResponseEntity<?>
	 */
	@PutMapping(value="/private/photo/publish/{idPhoto}/{idUsuario}/{value}")
	@ApiOperation(value="Publicar/Despublicar una fotografía",notes="Provee de un mecanismo para publicar/despublicar una fotografía")
	@ApiResponses(value={
			@ApiResponse(code=200,message="OK",response=ResponseError.class),
			@ApiResponse(code=401,message="Not Found",response=ResponseError.class),
			@ApiResponse(code=403,message="Not Found",response=ResponseError.class),
			@ApiResponse(code=404,message="Not Found",response=ResponseError.class),
			@ApiResponse(code=500,message="Internal Server Error",response=ResponseError.class)
		})
	public ResponseEntity<?> publishPhoto(@PathVariable Long idPhoto, @PathVariable Long idUsuario,@PathVariable Long value) throws PhotoNotFoundException,ErrorPublishPhotoException {
		
		if(this.manager.getPhoto(idPhoto)==null) {
			// No existe el id del álbum a eliminar
			throw new PhotoNotFoundException("No existe la fotografía a publicar/despublicar no existe");	
		}
		
		if(Boolean.FALSE.equals(this.manager.publishPhoto(idPhoto, idUsuario, value))) {
			throw new ErrorPublishPhotoException("Error al publicar/despublicar la fotografía");	
		} else {
			ResponseOperation<Object> response = new ResponseOperation<Object>();
			response.setStatus(HttpStatus.OK);
			response.setDescStatus("OK");
			response.setData(null);
			return ResponseEntity.status(HttpStatus.OK).body(response);		
		}
		
	}

}