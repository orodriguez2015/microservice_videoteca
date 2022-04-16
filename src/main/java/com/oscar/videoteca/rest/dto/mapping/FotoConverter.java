package com.oscar.videoteca.rest.dto.mapping;

import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.oscar.videoteca.rest.dto.FotoDTO;
import com.oscar.videoteca.rest.model.entity.Photo;

import lombok.RequiredArgsConstructor;

/**
 * Clase FotoConverter
 * @author <a href="mailto:oscar.rodriguezbrea@gmail.com">Óscar Rodríguez Brea</a>
 *
 */
@RequiredArgsConstructor
@Component
public class FotoConverter {

	@Autowired
	private ModelMapper mapper;
	
	/**
	 * Convierte un objeto de tipo Foto a un FotoDTO
	 * @param foto Foto
	 * @return FotoDTO
	 */
	public FotoDTO convertTo(Photo foto) {
		return this.mapper.map(foto,FotoDTO.class);
	}
	
	
	
}
