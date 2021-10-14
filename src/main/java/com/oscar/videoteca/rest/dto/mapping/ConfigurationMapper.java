package com.oscar.videoteca.rest.dto.mapping;

import org.modelmapper.ModelMapper;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;


/**
 * Clase ConfigurationMapper
 * @author <a href="mailto:oscarrbr@ext.inditex.com">Óscar Rodríguez Brea</a>
 *
 */
@Configuration
public class ConfigurationMapper {

	@Bean
	public ModelMapper getModelMapper() {
		return new ModelMapper();
	}
	
}
