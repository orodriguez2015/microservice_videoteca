package com.oscar.videoteca.rest.util;

/**
 * Enumerado que hace referencia a la visibilidad de una fotografía
 * @author <a href="mailto:oscar.rodriguezbrea@gmail.com">Óscar Rodríguez</a>
 *
 */
public enum PhotoVisibilityEnum {
	
	PRIVATE_PHOTO(0),PUBLISHED_PHOTO(1),ALL_PHOTOS(2);
	/** Código */
	private Integer codigo;
	
	/**
	 * Constructor
	 * @param codigo Integer
	 */
	private PhotoVisibilityEnum(Integer codigo) {
		this.codigo = codigo;
	}
	
}
