package com.oscar.videoteca.rest.model.repository;

import org.springframework.data.jpa.repository.JpaRepository;

import com.oscar.videoteca.rest.model.entity.Album;

/**
 * Interface AlbumRepository
 * @author <a href="mailto:oscarrbr@ext.inditex.com">Óscar Rodríguez Brea</a>
 *
 */
public interface AlbumRepository extends JpaRepository<Album,Long>{

}
