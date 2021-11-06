package com.oscar.videoteca.rest.model.repository;

import org.springframework.data.jpa.repository.JpaRepository;

import com.oscar.videoteca.rest.model.entity.Videoteca;

/**
 * Repository para la entidad Videoteca
 * @author <a href="mailto:oscarrbr@ext.inditex.com">Óscar Rodríguez Brea</a>
 *
 */
public interface VideotecaRepository extends JpaRepository<Videoteca, Long> {

}
