package com.oscar.videoteca.rest.dto.mapping;

import java.util.List;
import java.util.stream.Collectors;

import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.oscar.videoteca.rest.dto.VideotecaDTO;
import com.oscar.videoteca.rest.model.entity.Videoteca;

import lombok.RequiredArgsConstructor;

/**
 * Clase de conversión para la entidad Videoteca
 * @author <a href="mailto:oscarrbr@ext.inditex.com">Óscar Rodríguez Brea</a>
 *
 */
@Component
@RequiredArgsConstructor
public class VideotecaConverter {

	@Autowired
	private ModelMapper modelMapper; 
	
	
	/**
	 * Convierte un objetyo de tipo Videoteca en un VideotecaDTO
	 * @param videoteca Videoteca
	 * @return VideotecaDTO
	 */
	public VideotecaDTO convertTo(Videoteca videoteca) {
		return modelMapper.map(videoteca,VideotecaDTO.class);
	}
	
	
	/**
	 * Convierte un List<Videoteca> en un List<VideotecaDTO>
	 * @param videotecas List<Videoteca>
	 * @return List<VideotecaDTO>
	 */
	public List<VideotecaDTO> convertTo(List<Videoteca> videotecas) {
		 return videotecas.stream().map(v -> this.convertTo(v)).collect(Collectors.toList());
	}
	
	
}