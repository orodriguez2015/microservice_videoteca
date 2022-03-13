package com.oscar.videoteca.rest.controller;

import java.io.IOException;
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

import com.oscar.videoteca.rest.exception.SaveFileException;
import com.oscar.videoteca.rest.exception.api.ResponseOperation;
import com.oscar.videoteca.rest.manager.FotoManager;

/**
 * Controller que recibe peticiones de upload de fotografías para un determinado álbum fotográfico
 * @author <a href="mailto:oscar.rodriguezbrea@gmail.com">Óscar Rodríguez Brea</a>
 *
 */
@RestController
public class FileAlbumUploadController {
	
	@Autowired
	private FotoManager fotoManager;

	  @PostMapping("/private/album/{idAlbum}/{idUsuario}")
	  public ResponseEntity<?> uploadFiles(@RequestParam("ficheros")MultipartFile[] files,@RequestParam("idAlbum") String idAlbum,@RequestParam("idUsuario") String idUsuario){
        try{
            List<String> fileNames = new ArrayList<>();
           
           
            Arrays.asList(files).stream().forEach(file->{
            	 	
            	try { 	
            		System.out.println("Nombre = " + file.getName() + ", size= " + file.getSize());
                
            		fotoManager.saveFoto(file,Long.parseLong(idAlbum),Long.parseLong(idUsuario));
            		
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
}
