package com.oscar.videoteca.rest.dto.mapping;

import java.util.List;
import java.util.stream.Collectors;

import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.oscar.videoteca.rest.dto.AlbumDTO;
import com.oscar.videoteca.rest.model.entity.Album;

import lombok.RequiredArgsConstructor;

/**
 * Clase de conversión para la entidad Album
 * @author <a href="mailto:oscarrbr@ext.inditex.com">Óscar Rodríguez Brea</a>
 *
 */
@Component
@RequiredArgsConstructor
public class AlbumConverter {

	@Autowired
	private ModelMapper mapper;
	
	/**
	 * Convierte un objeto de tipo Album a un AlbumDTO
	 * @param album Album
	 * @return AlbumDTO
	 */
	public AlbumDTO convertTo(Album album) {
		return this.mapper.map(album,AlbumDTO.class);
	}
	
	
	/**
	 * Convierte una List<Album> a un List<AlbumDTO>
	 * @param albumes List<Album>
	 * @return List<AlbumDTO>
	 */
	public List<AlbumDTO> converTo(List<Album> albumes) {
		return albumes.stream().map(a->convertTo(a)).collect(Collectors.toList());		
	}
	
}
