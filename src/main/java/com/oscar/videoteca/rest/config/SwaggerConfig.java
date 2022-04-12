package com.oscar.videoteca.rest.config;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;

import springfox.documentation.builders.ApiInfoBuilder;
import springfox.documentation.builders.PathSelectors;
import springfox.documentation.builders.RequestHandlerSelectors;
import springfox.documentation.service.ApiInfo;
import springfox.documentation.service.Contact;
import springfox.documentation.spi.DocumentationType;
import springfox.documentation.spring.web.plugins.Docket;
import springfox.documentation.swagger2.annotations.EnableSwagger2;

/**
 * Clase de tipo Configuration que se habilita la generación de documentación de tipo swagger para las clases
 * existentes dentro del paquete "com.openwebinars.rest.controller"
 * @author oscar
 *
 */
@Configuration
@EnableSwagger2
public class SwaggerConfig {

	@Bean
	 public Docket api() {
		 return new Docket(DocumentationType.SWAGGER_2)
				 .select()
				 .apis(
						 RequestHandlerSelectors.basePackage("com.oscar.videoteca.rest.controller"))
				 .paths(PathSelectors.any())				 
				 .build().apiInfo(apiInfo());
	 } 
	
	
	@Bean
	public ApiInfo apiInfo() {
		return new ApiInfoBuilder().title("API Videoteca").description("API web services de la aplicación videoteca").version("1.0")
				.contact(new Contact("Öscar Rodríguez Brea", null,"oscar.rodriguezbrea@gmail.com"))
				.build();
		
	}
	
	
	
}

