package com.oscar.videoteca.rest.controller;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import com.oscar.videoteca.rest.exception.SaveVideoException;
import com.oscar.videoteca.rest.exception.api.ResponseOperation;
import com.oscar.videoteca.rest.manager.VideoManager;

/**
 * Controller para recibir peticiones de upload y descarga de un vídeo
 * @author <a href="mailto:oscar.rodriguezbrea@gmail.com">Óscar Rodríguez</a>
 *
 */
@RestController
public class VideoUploadController {
	
	@Autowired
	private VideoManager videoManager;

	/**
	 * Método que permite la subida de vídeos al servior
	 * @param files MultipartFile[]
	 * @param idVideoteca Id de la videoteca
	 * @param idUsuario Id del usuario
	 * @return ResponseEntity<?>
	 */
	@PostMapping("/private/video/{idVideoteca}/{idUsuario}")
	public ResponseEntity<?> uploadVideo(@RequestParam("ficheros")MultipartFile[] files,@RequestParam("idVideoteca") String idVideoteca,@RequestParam("idUsuario") String idUsuario){
        try{
            List<String> fileNames = new ArrayList<>();
        
            Arrays.asList(files).stream().forEach(file->{            	 	
            	try { 	                
            		videoManager.saveVideo(file,Long.valueOf(idVideoteca),Long.valueOf(idUsuario));
            		
            	}catch(SaveVideoException e) {
            		e.printStackTrace();
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
			response.setDescStatus("Error al subir video al servidor: " + e.getMessage());
            return ResponseEntity.status(HttpStatus.EXPECTATION_FAILED).body(response);
        }
    }

	
}
