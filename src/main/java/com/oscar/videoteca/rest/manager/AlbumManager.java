package com.oscar.videoteca.rest.manager;

import java.util.List;

import com.oscar.videoteca.rest.dto.AlbumDTO;

/**
 * Interface AlbumManager
 * @author <a href="mailto:oscarrbr@ext.inditex.com">Óscar Rodríguez Brea</a>
 *
 */
public interface AlbumManager {

	/**
	 * Recupera los álbumes fotográficos públicos
	 * @return List<AlbumDTO>
	 */
	List<AlbumDTO> getAlbumesPublicos();
}
