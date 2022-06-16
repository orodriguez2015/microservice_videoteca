package com.oscar.videoteca.rest.controller;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.io.ByteArrayResource;
import org.springframework.core.io.Resource;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import com.google.common.io.Files;
import com.oscar.videoteca.rest.dto.PhotoDTO;
import com.oscar.videoteca.rest.exception.PhotoNotFoundException;
import com.oscar.videoteca.rest.exception.SaveFileException;
import com.oscar.videoteca.rest.exception.api.ResponseOperation;
import com.oscar.videoteca.rest.manager.PhotoManager;

/**
 * Controller que recibe peticiones de upload de fotografías para un determinado álbum fotográfico
 * @author <a href="mailto:oscar.rodriguezbrea@gmail.com">Óscar Rodríguez Brea</a>
 *
 */
@RestController
public class FileAlbumUploadController {
	
	@Autowired
	private PhotoManager fotoManager;
	
	
	@PostMapping("/private/album/{idAlbum}/{idUsuario}")
	public ResponseEntity<?> uploadFiles(@RequestParam("ficheros")MultipartFile[] files,@RequestParam("idAlbum") String idAlbum,@RequestParam("idUsuario") String idUsuario){
        try{
            List<String> fileNames = new ArrayList<>();
        
            Arrays.asList(files).stream().forEach(file->{            	 	
            	try { 	                
            		fotoManager.savePhoto(file,Long.parseLong(idAlbum),Long.parseLong(idUsuario));
            		
            	}catch(IOException e) {
            		
            	}catch(SaveFileException e) {
            		
            	}
                fileNames.add(file.getOriginalFilename());
            });

                        
            ResponseOperation<Object> response = new ResponseOperation<Object>();
			response.setStatus(HttpStatus.OK);
			response.setDescStatus("OK");
			response.setData(null);
			return ResponseEntity.status(HttpStatus.OK).body(response);		
        
            
        }catch (Exception e){   
            ResponseOperation<Object> response = new ResponseOperation<Object>();
			response.setStatus(HttpStatus.EXPECTATION_FAILED);
			response.setDescStatus("Error al subir fotografías al servidor: " + e.getMessage());
         
            return ResponseEntity.status(HttpStatus.EXPECTATION_FAILED).body(response);
        }
    }
	  
	/**
	 * Permite descargar una fotografía del servior
	 * @param idPhoto Id del foto
	 * @return ResponseEntity<Resource>
	 * @throws IOException
	 */
	@RequestMapping(value = "/download/photo/{idPhoto}", method = RequestMethod.GET,produces = { MediaType.APPLICATION_OCTET_STREAM_VALUE})
	public ResponseEntity<Resource> downloadPhoto(@RequestParam Long idPhoto) throws IOException, PhotoNotFoundException {
		
		PhotoDTO fotoDTO = fotoManager.getPhoto(idPhoto);
		if(fotoDTO!=null) {
		      File file = new File(fotoDTO.getRuta());
		      HttpHeaders headers = new HttpHeaders();
		      headers.add("Cache-Control", "no-cache, no-store, must-revalidate");
		      headers.add("Pragma", "no-cache");
		      headers.add("Expires", "0");
	
		      ByteArrayResource resource = new ByteArrayResource(Files.toByteArray(file));
	
		      return ResponseEntity.ok()
		              .headers(headers)
		              .contentLength(file.length())
		              .contentType(MediaType.parseMediaType("application/octet-stream"))
		              .body(resource);
		}
		return null;

	}
		

}
