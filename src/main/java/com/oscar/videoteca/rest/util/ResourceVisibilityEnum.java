package com.oscar.videoteca.rest.util;

/**
 * Enumerado que hace referencia a la visibilidad de un recursos (fotografía, vídeo, etc...)
 * @author <a href="mailto:oscar.rodriguezbrea@gmail.com">Óscar Rodríguez</a>
 *
 */
public enum ResourceVisibilityEnum {
	
	PRIVATE(0),PUBLISHED(1),ALL(2),NONE(3);
	/** Código */
	private Integer codigo;
	
	/**
	 * Devuelve el código 
	 * @return Integer
	 */
	public Integer getCodigo() {
		return this.codigo;
	}
	
	/**
	 * Constructor
	 * @param codigo Integer
	 */
	private ResourceVisibilityEnum(Integer codigo) {
		this.codigo = codigo;
	}
	
}
