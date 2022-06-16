package com.oscar.videoteca.rest.dto.mapping;

import java.util.List;
import java.util.stream.Collectors;

import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.oscar.videoteca.rest.dto.PhotoDTO;
import com.oscar.videoteca.rest.model.entity.Photo;

import lombok.RequiredArgsConstructor;

/**
 * Clase FotoConverter
 * @author <a href="mailto:oscar.rodriguezbrea@gmail.com">Óscar Rodríguez Brea</a>
 *
 */
@RequiredArgsConstructor
@Component
public class PhotoConverter {

	@Autowired
	private ModelMapper mapper;
	
	/**
	 * Convierte un objeto de tipo Foto a un FotoDTO
	 * @param foto Foto
	 * @return FotoDTO
	 */
	public PhotoDTO convertTo(Photo foto) {
		return this.mapper.map(foto,PhotoDTO.class);
	}
	
	/**
	 * Convierte una colección de List<Photo> en un List<PhotoDTO>
	 * @param photos
	 * @return
	 */
	public List<PhotoDTO> convertTo(List<Photo> photos) {
		return photos.stream().map(p->convertTo(p)).collect(Collectors.toList());
	}
	
	
}
