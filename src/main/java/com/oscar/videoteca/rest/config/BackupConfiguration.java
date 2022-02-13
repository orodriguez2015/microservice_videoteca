package com.oscar.videoteca.rest.config;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

import lombok.Getter;
import lombok.Setter;


/**
 * Clase que recupera la configuración de las carpeta de backup para las fotografías
 * de los álbumes, y para los vídeos de una videoteca
 * 
 * @author <a href="mailto:oscar.rodriguezbrea@gmail.com">Óscar Rodríguez Brea</a>
 *
 */
@Getter
@Setter
@Component
@ConfigurationProperties(prefix = "backup.folder")
public class BackupConfiguration {
	/** Ruta de la carpeta de backup para álbumes */
	private String album;
	/** Ruta de la carpeta de backup para vídeos */
	private String video;
	
}
