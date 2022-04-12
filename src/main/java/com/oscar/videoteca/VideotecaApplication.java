
package com.oscar.videoteca;

import java.io.File;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.web.servlet.config.annotation.ResourceHandlerRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;

import com.oscar.videoteca.constants.Constants;
import com.oscar.videoteca.rest.config.BackupConfiguration;

@SpringBootApplication
public class VideotecaApplication implements WebMvcConfigurer {

	@Autowired
	private BackupConfiguration backupConfiguration;
	
	public static void main(String[] args) {
		SpringApplication.run(VideotecaApplication.class, args);
	}

	
	/**
	 * Add handlers to serve static resources such as images, js, and, css
	 * files from specific locations under web application root, the classpath,
	 * and others.
	 * @see ResourceHandlerRegistry
	 */
	@Override
    public void addResourceHandlers(ResourceHandlerRegistry registry) {
		// Se configura el acceso a la carpeta de imágenes de los álbumes de los usuarios en un directorio externo
		registry.addResourceHandler("/images/**").addResourceLocations(Constants.FILE.concat(backupConfiguration.getAlbum()).concat(String.valueOf(File.separatorChar)));
		//Se configura el acceso a la carpeta de vídeos de las videotecas de los usuarios en un directorio externo
		registry.addResourceHandler("/videos/**").addResourceLocations(Constants.FILE.concat(backupConfiguration.getVideo()).concat(String.valueOf(File.separatorChar)));
    }
}
