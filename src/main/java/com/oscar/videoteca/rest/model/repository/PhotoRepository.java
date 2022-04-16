package com.oscar.videoteca.rest.model.repository;

import org.springframework.data.jpa.repository.JpaRepository;

import com.oscar.videoteca.rest.model.entity.Photo;


/**
 * Interface FotoRepository con operación de acceso a datos contra 
 * la tabla foto
 * @author <a href="mailto:oscar.rodriguezbrea@gmail.com">Óscar Rodríguez Brea</a>
 *
 */
public interface PhotoRepository extends JpaRepository<Photo,Long> {

}
