package com.oscar.videoteca.rest.dto.mapping;

import java.util.List;
import java.util.stream.Collectors;

import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.oscar.videoteca.rest.dto.CreateVideotecaDTO;
import com.oscar.videoteca.rest.dto.VideoDTO;
import com.oscar.videoteca.rest.dto.VideotecaDTO;
import com.oscar.videoteca.rest.model.entity.Video;
import com.oscar.videoteca.rest.model.entity.Videoteca;

import lombok.RequiredArgsConstructor;

/**
 * Clase de conversión para la entidad Videoteca
 * @author <a href="mailto:oscar.rodriguezbrea@gmail.com">Óscar Rodríguez Brea</a>
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
	 * Convierte un objeto de tipo CreateVideotecaDTO a Videoteca
	 * @param create
	 * @return
	 */
	public Videoteca convertTo(CreateVideotecaDTO create) {
		return modelMapper.map(create,Videoteca.class);
	}
	
	
	/**
	 * Convierte un List<Videoteca> en un List<VideotecaDTO>
	 * @param videotecas List<Videoteca>
	 * @return List<VideotecaDTO>
	 */
	public List<VideotecaDTO> convertTo(List<Videoteca> videotecas) {
		 return videotecas.stream().map(v -> this.convertTo(v)).collect(Collectors.toList());
	}
	
	
	/**
	 * Convierte un objeto de tipo Video en un VideoDTO
	 * @param video Video
	 * @return VideoDTO
	 */
	public VideoDTO convertTo(Video video) {
		return modelMapper.map(video,VideoDTO.class);
	}
	
	
	public List<VideoDTO> convertToListVideoDTO(List<Video> videos) {
		return videos.stream().map(v->this.convertTo(v)).collect(Collectors.toList());
	}
	
	
	
}