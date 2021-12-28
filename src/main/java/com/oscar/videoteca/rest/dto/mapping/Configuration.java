package com.oscar.videoteca.rest.dto.mapping;

import org.modelmapper.ModelMapper;
import org.springframework.context.annotation.Bean;


/**
 * Clase ConfigurationMapper
 * @author <a href="mailto:oscarrbr@ext.inditex.com">Óscar Rodríguez Brea</a>
 *
 */
@org.springframework.context.annotation.Configuration
public class Configuration {

	@Bean
	public ModelMapper getModelMapper() {
		return new ModelMapper();
	}

	
}
