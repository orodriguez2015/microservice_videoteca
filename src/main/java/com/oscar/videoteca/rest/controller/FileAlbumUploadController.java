package com.oscar.videoteca.rest.controller;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import com.oscar.videoteca.rest.exception.api.ResponseOperation;

@RestController
public class FileAlbumUploadController {

	  @PostMapping("/private/album/{idAlbum}/{idUsuario}")
	  public ResponseEntity<?> uploadFiles(@RequestParam("ficheros")MultipartFile[] files){
        try{
            List<String> fileNames = new ArrayList<>();

            Arrays.asList(files).stream().forEach(file->{
            	
            	System.out.println("Nombre = " + file.getName() + ", size= " + file.getSize());
                //fileService.save(file);
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
			response.setDescStatus("Error al subir fotografías al servidor");
         
            return ResponseEntity.status(HttpStatus.EXPECTATION_FAILED).body(response);
        }
    }
}
