package com.oscar.videoteca.rest.dto.mapping;

import org.modelmapper.ModelMapper;
import org.springframework.context.annotation.Bean;
import org.springframework.web.servlet.config.annotation.CorsRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;


/**
 * Clase ConfigurationMapper
 * @author <a href="mailto:oscarrbr@ext.inditex.com">Óscar Rodríguez Brea</a>
 *
 */
//@org.springframework.context.annotation.Configuration
@org.springframework.context.annotation.Configuration
public class Configuration {

	@Bean
	public ModelMapper getModelMapper() {
		return new ModelMapper();
	}

	
	/**
	 * Se devuelve un WebMvcConfigurer que permite la implementación de varios callback, entre ellos
	 * el addCorsMappings, en que se indicará los orígenes desde los que se podrá invocar a los microservicios
	 * desplegados en Spring Boot
	 * @return WebMvcConfigurer
	 */
//	@Bean
//	public WebMvcConfigurer coorsMapping() {
//		
//		return new WebMvcConfigurer() {
//			
//			public void addCorsMappings(CorsRegistry registry) {
//				// Con esta opción se indica que se permite acceder desde cualquier url al microservicio Spring Boot
//							
//				// Opcion 2 => Acceso a determinada url y desde determinadas direcciones
//				registry.addMapping("/videoteca/**").allowedOrigins("*").allowedMethods("GET","PUT","POST","DELETE").maxAge(3600);
//			}
//			
//		};
//	}
	
	
}
